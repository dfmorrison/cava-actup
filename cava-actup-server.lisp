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

(ql:quickload '(:alexandria :iterate :cl-interpol :cl-ppcre :usocket-server :babel
                :cl-json :bordeaux-threads :local-time :uiop :vom :trivial-backtrace))

(interpol:enable-interpol-syntax)

(defpackage :cava
  (:use :common-lisp :alexandria :iterate)
  (:local-nicknames (:us :usocket)
                    (:bb :babel)
                    (:js :json)
                    (:re :ppcre)
                    (:lt :local-time)
                    (:ui :uiop)
                    (:tb :trivial-backtrace))
  (:export #:run #:recency-model #:recency-frequency-model #:sequential-model))

(in-package :cava)

(vom:config :cava :info)

(load (merge-pathnames #P"act-up-v1_3_1" *load-truename*))

(defparameter *default-port*
  (or (ignore-errors (parse-integer (ui:getenvp "CAVA_ACTUP_PORT"))) 9017))

(defparameter +default-logfile-template+
  '("cava-data-" (:year 4) #\- (:month 2) #\- (:day 2) #\- (:hour 2) (:min 2)  (:sec 2)))

(defparameter *logfile*
  (merge-pathnames (or (ui:getenvp "CAVA_ACTUP_LOGFILE")
                       (lt:format-timestring nil (lt:now)
                                             :format +default-logfile-template+))
                   (merge-pathnames (make-pathname :type "json") *load-truename*)))

(defparameter *using-numeric-ids* nil)
(defparameter *last-click* 0)
(defparameter *time-origin* nil)

(defun reset (&rest params &key &allow-other-keys)
  (setf *using-numeric-ids* nil)
  (setf *last-click* 0)
  (setf *time-origin* nil)
  (init-memory)
  (init-similarities)
  (parameter :ol nil)
  (parameter :ans 0)
  (iter (for (key val) :on params :by #'cddr)
        (parameter key val)))

(defun place-into-bins (alist &optional (bin-count 6))
  (iter (with values := (mapcar #'cdr alist))
        (with min-val := (apply #'min values))
        (with delta := (/ (- (apply #'max values) min-val) (float bin-count)))
        (with thresholds := (iter (for i :from 1 :to 5)
                                  (collect (+ min-val (* i delta)))))
        (for (key . val) :in alist)
        (for level := (position-if (lambda (v) (>= val v)) thresholds :from-end t))
        (when level
          (collect (cons key (+ level 1))))))

(defun past-model (id time)
  (learn `((node ,id)))
  (actr-time (- time (actr-time)))
  (place-into-bins (iter (for (nil chunk) :in-hashtable *memory*)
                         (for content := (chunk-content chunk))
                         (when (eq (caar content) 'node)
                           (collect (cons (cadar content) (exp (activation chunk))))))))

(defparameter *history* (list nil nil))

(defun future-model (id time)
  (push id *history*)
  (labels ((lags (&optional include-current)
             (let ((tags '(current lag1 lag2)))
               (unless include-current
                 (pop tags))
               (mapcar #'list tags *history*))))
    (prog1
        (and (second *history*)
             (place-into-bins (mapcar (curry #'apply #'cons)
                                      (third (multiple-value-list
                                              (blend-vote (lags) 'current))))))
      (learn (lags t))
      (actr-time (- time (actr-time)))
      (pop (cddr *history*)))))

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

(defun run-model (json)
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
    (unless id
      (error "IDs cannot be nil"))
    (vom:debug "Calling model on ~S, ~S" sym offset)
    (let ((past (past-model sym offset))
          (future (future-model sym offset)))
      (vom:debug "Past model returned ~S" past)
      (vom:debug "Future model returned ~S" future)
      (unless (and (listp past) (listp future))
        (error "Model functions returned unexpected values ~S, ~S" past future))
      `((:past . ,(iter (for (key . val) :in past)
                        (collect (cons (symbol-name key) val))))
        (:future . ,(iter (for (key . val) :in future)
                          (collect (cons (symbol-name key) val))))
        (:timestamp . ,ts)))))

(defparameter *log-lock* (bt:make-lock "log lock"))

(defun remote-host ()
  (let ((h us:*remote-host*))
    (cond ((stringp h) h)
          ((and (vectorp h) (eql (length h) 4) (every #'integerp h))
           (format nil "~{~D~^.~}" (coerce h 'list)))
          (t "unknown host"))))

(defun write-log (message response)
  (let ((time (lt:now)))
    (bt:with-lock-held (*log-lock*)
      (with-open-file (s *logfile* :direction :output :if-exists :append :if-does-not-exist :create)
        (format s #?'{"when": "~A", "unix-time": ~D.~D, "remote": "~A", "message": ~A, "response": ~A}~%'
                time (lt:timestamp-to-unix time) (round (lt:nsec-of time) 1000)
                (remote-host) message response)
        (force-output s)))))            ; probably redundant?

(defun receive-udp (buffer)
  (vom:debug1 "Received bytes: ~S" buffer)
  (let ((msg (string-trim #?" \n\r"
                          (bb:octets-to-string buffer :encoding :utf-8))))
    (vom:debug "Received: ~S" msg)
    (let (result)
      (handler-case
          (let* ((js:*json-identifier-name-to-lisp* #'js:simplified-camel-case-to-lisp)
                 (*read-default-float-format* 'double-float)
                 (json (with-input-from-string (s msg)
                         (js:decode-json-strict s))))
            (vom:debug1 "Decoded message: ~S" json)
            (setf result (run-model json)))
        (error (e)
          (vom:error "Error handling message ~A: ~A (~:*~S)" msg e)
          (setf result `((:error . ,(string (type-of e)))
                         (:message . ,msg)
                         (:description . ,(format nil "~A" e))
                         (:backtrace . ,(tb:print-backtrace e :output nil))))))
      (vom:debug1 "Result: ~S" result)
      (let ((response (js:encode-json-to-string result)))
        ;; seems a shame I can't figure out how to get CL-JSON to do it this way
        (setf response (re:regex-replace "\":null," response "\":{},"))
        (vom:debug "Replying: ~S" response)
        (write-log msg response)
        (bb:string-to-octets response)))))

(defun run (&optional (port *default-port*))
  (handler-case (progn
                  (open *logfile* :direction :output
                                  :if-exists :append
                                  :if-does-not-exist :create)
                  (vom:info "CAVA-ACT-UP-server listening on port ~D" port)
                  (reset)
                  (vom:debug "ACT-UP initialized")
                  (us:socket-server nil port #'receive-udp nil :protocol :datagram))
    (error (e)
      #+SBCL
      (when (and (typep e 'usocket:socket-error)
                 (typep (usocket::usocket-real-error e) 'sb-sys:interactive-interrupt)
                 (zerop (usocket::usocket-errno e)))
        (setf e nil))
      (when e
        (vom:crit "An error occurred at top level: ~A (~:*~S)~%~A"
                  e (tb:print-backtrace e :output nil)))
      (ui:quit (if e 1 0)))))
