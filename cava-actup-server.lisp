;;; Copyright (c) 2022 Carnegie Mellon University
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;; software and associated documentation files (the "Software"), to deal in the Software
;;; without restriction, including without limitation the rights to use, copy, modify,
;;; merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all copies
;;; or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
;;; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
;;; OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(ql:quickload '(:alexandria :iterate :cl-interpol :usocket-server :babel :cl-json
                :uiop :vom :trivial-backtrace))

(interpol:enable-interpol-syntax)

(defpackage :cava
  (:use :common-lisp :alexandria :iterate)
  (:local-nicknames (:u :usocket)
                    (:b :babel)
                    (:j :json)
                    (:tb :trivial-backtrace))
  (:export #:run #:recency-model #:recency-frequency-model))

(in-package :cava)

(vom:config :cava :info)

(load (merge-pathnames #P"act-up-v1_3_1" *load-truename*))

(defparameter *default-port* 9017)

(defparameter *using-numeric-ids* nil)
(defparameter *last-click* 0)
(defparameter *time-origin* nil)

(defun reset (&rest params &key &allow-other-keys)
  (setf *using-numeric-ids* nil)
  (setf *last-click* 0)
  (setf *time-origin* nil)
  (init-memory)
  (parameter :ol nil)
  (iter (for (key val) :on params :by #'cddr)
        (parameter key val)))

(defun time-offset (timestamp)
  (check-type timestamp real)
  (when (< timestamp *last-click*)
    (error "Clicks appear to be arriving out of time sequence (~A, ~A)"
           *last-click* timestamp))
  (setf *last-click* timestamp)
  (unless *time-origin*
    (setf *time-origin* (- timestamp 1)))
  (let ((result (- timestamp *time-origin*)))
    (assert (> result 0))
    result))

(defun call-model (model-function json)
  (let* ((id (cdr (or (assoc :id json)
                      (error "No ID supplied for click"))))
         ;; It is important that the timestamp be double precision, as a single
         ;; precision float lacks sufficient precision to represent times to the
         ;; second in 2022.
         (sym (cond ((integerp id)
                     (unless *using-numeric-ids*
                       (vom:warn
                        "Numeric IDs in use, that will converted to strings on output (~D)"
                        id)
                       (setf *using-numeric-ids* t))
                     (intern (format nil "~D" id)))
                    ((or (not (stringp id)) (zerop (length id)))
                     (error "IDs must be non-empty strings or integers: ~S" id))
                    ((equal id "NIL") :nil)
                    (t (intern id))))
         (ts (cdr (or (assoc :timestamp json)
                      (error "No timestamp supplied for click"))))
         (offset (time-offset ts)))
    (vom:debug "Calling ~S on ~S, ~S" model-function sym offset)
    (let ((result (funcall model-function sym offset)))
      (vom:debug "Model function ~S returned ~S" model-function result)
      (unless (consp result)
        (error "Model function ~S returned an unexpected value ~S" model-function result))
      `((:levels . ,(iter (for (key . val) :in result)
                          (collect (cons (symbol-name key) val))))
        (:timestamp . ,ts)))))

(defun receive-udp (buffer model-function)
  (vom:debug1 "Received bytes: ~S" buffer)
  (let ((msg (string-trim #?" \n\r"
                          (b:octets-to-string buffer :encoding :utf-8))))
    (vom:debug "Received: ~S" msg)
    (let (result)
    (handler-case
        (let* ((j:*json-identifier-name-to-lisp* #'j:simplified-camel-case-to-lisp)
               (*read-default-float-format* 'double-float)
               (json (with-input-from-string (s msg)
                       (j:decode-json-strict s))))
          (vom:debug1 "Decoded message: ~S" json)
          (setf result (call-model model-function json)))
      (error (e)
        (vom:error "Error handling message ~A: ~A (~:*~S)" msg e)
        (setf result `((:error . ,(string (type-of e)))
                       (:message . ,msg)
                       (:description . ,(format nil "~A" e))
                       (:backtrace . ,(tb:print-backtrace e :output nil))))))
      (vom:debug1 "Result: ~S" result)
      (let ((response (j:encode-json-to-string result)))
        (vom:debug "Replying: ~S" response)
        (b:string-to-octets response)))))



;;; ACT-UP models

(defun recency-only-activation (chunk &optional (trace *verbose*))
  "Computes activation according to power law recency, no practicefrequency. If *decay* is
zero the result will always be zero."
  (when *optimized-learning*
    (error "recency-only-activation cannot be used with optimized learning"))
  (let ((activation (* (- *decay*) (log (- (get-time) (nth 0 (chunk-references chunk)))))))
    (when *noise*
      (incf activation (noise *noise*)))
    (when trace
      (format t "Calculating Chunk ~A (recency only) Activation ~6,3F.~%"
              (chunk-name chunk) activation))
    activation))

(defun place-into-bins (alist &optional (bin-count 6))
  (iter (with values := (mapcar #'cdr alist))
        (with min-val := (apply #'min values))
        (with delta := (/ (- (apply #'max values) min-val) (float bin-count)))
        (with thresholds := (iter (for i :from 1 :to 5)
                                  (collect (+ min-val (* i delta)))))
        (for (key . val) :in alist)
        (for level := (position-if (lambda (v) (>= val v)) thresholds :from-end t))
        (when level
          (collect (cons key (+ level 2))))))

(defun simple-model (id time activation-function)
  (learn `((:node ,id)))
  (actr-time (- time (actr-time)))
  (place-into-bins (mapcar (lambda (c)
                             (cons (cadar (chunk-content c))
                                   (exp (funcall activation-function c))))
                           (hash-table-values *memory*))))

(defun recency-model (id time)
  (simple-model id time #'recency-only-activation))

(defun recency-frequency-model (id time)
  (simple-model id time #'activation))



(defun run (model-function &key (model-parameters) (port *default-port*))
  (handler-case (progn
                  (vom:info "CAVA-ACT-UP-server listening on port ~D (~A)"
                            port model-function)
                  (apply #'reset model-parameters)
                  (vom:debug "ACT-UP initialized")
                  (u:socket-server nil port #'receive-udp (list model-function)
                                   :protocol :datagram))
    (error (e)
      #+SBCL
      (when (and (typep e 'usocket:socket-error)
                 (typep (usocket::usocket-real-error e) 'sb-sys:interactive-interrupt)
                 (zerop (usocket::usocket-errno e)))
        (setf e nil))
      (when e
        (vom:crit "An error occurred at top level: ~A (~:*~S)~%~A"
                  e (tb:print-backtrace e :output nil)))
      (uiop:quit (if e 1 0)))))
