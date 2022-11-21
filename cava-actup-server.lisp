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

(defparameter *activation-fn* (symbol-function 'activation))

(defparameter *capture-activations* nil)

(defvar *activations* nil)

(fmakunbound 'activation)               ; suppress the redefinition warning

(defun activation (chunk &optional (trace *verbose*))
  (let ((result (funcall *activation-fn* chunk trace)))
    (when *capture-activations*
      (push (cons (chunk-content chunk) result) *activations*))
    result))

(defmacro with-activations (&body body)
  `(%with-activations (lambda () ,@body)))

(defun %with-activations (thunk)
  (let* ((*activations* nil)
         (*capture-activations* t)
         (result (funcall thunk)))
    (values result (nreverse *activations*))))

(defparameter *init-file* (merge-pathnames #P"init_data_002.lisp" *load-truename*))

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

(defun integer-environment-variable-value (name &optional (default 0))
  (check-type default integer)
  (if-let ((value (ui:getenv name)))
    (handler-case (parse-integer value)
      (error (e)
        (vom:warn "Couldn't parse value of environment variable ~A, ~S, as an integer, ~
                   using default of ~:D instead (~S)"
                  name value default e)
        default))
    default))

(defparameter *initial-data* nil)

(defparameter *current-task* nil)

(defun reset (&key params (init-file *init-file*))
  (setf *using-numeric-ids* nil)
  (setf *last-click* 0)
  (setf *time-origin* nil)
  (init-memory)
  (init-similarities)
  (parameter :ol nil)
  (parameter :ans 0)
  (iter (for (key val) :on params :by #'cddr)
        (parameter key val))
  (setf *current-task* nil)
  (with-open-file (in init-file)
    (setf *initial-data* (read in)))
  (actr-time 1))

(defun place-into-bins (alist limit &optional (bins 5))
  (when (cdr alist)
    (setf alist (stable-sort alist #'> :key #'cdr))
    (setf alist (subseq alist 0 (min limit (length alist))))
    (iter (with min-val := (cdar (last alist)))
          (with delta := (/ (- (cdr (first alist)) min-val) (- bins 1)))
          (for (key . val) :in alist)
          (collect (cons key (if (> delta 0) (1+ (floor (- val min-val) delta)) 1))))))

(defun past-model (id time)
  (vom:debug "Calling past-model on ~S, ~S" id time)
  (learn `((node ,id)))
  (actr-time (- time (actr-time)))
  (place-into-bins (iter (for (nil chunk) :in-hashtable *memory*)
                         (for content := (chunk-content chunk))
                         (when (eq (caar content) 'node)
                           (collect (cons (cadar content) (exp (activation chunk))))))
                   (integer-environment-variable-value "CAVA_PAST_MAX_HIGHLIGHTS" 5)))

(defparameter *history* (list nil nil))

(defun future-model (id time)
  (vom:debug "Calling future-model on ~S, ~S" id time)
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
                                              (blend-vote (lags) 'current))))
                              (integer-environment-variable-value
                               "CAVA_FUTURE_MAX_HIGHLIGHTS" 3)))
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
    (setf *time-origin* (- timestamp 2)))
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
    (multiple-value-bind (past past-activations)
        (with-activations (past-model sym offset))
      (multiple-value-bind (future future-activations)
          (with-activations (future-model sym offset))
        (vom:debug "Past model returned ~S" past)
        (vom:debug "Future model returned ~S" future)
        (unless (and (listp past) (listp future))
          (error "Model functions returned unexpected values ~S, ~S" past future))
        (values `((:past . ,(iter (for (key . val) :in past)
                                  (collect (cons (symbol-name key) val))))
                  (:future . ,(iter (for (key . val) :in future)
                                    (collect (cons (symbol-name key) val))))
                  (:timestamp . ,ts))
                (list past-activations future-activations))))))

(defparameter *log-lock* (bt:make-lock "log lock"))

(defun remote-host ()
  (let ((h us:*remote-host*))
    (cond ((stringp h) h)
          ((and (vectorp h) (eql (length h) 4) (every #'integerp h))
           (format nil "~{~D~^.~}" (coerce h 'list)))
          (t "unknown host"))))

(defun write-log (message response activations)
  (dolist (alist activations)
    (dolist (pair alist)
      (rplaca pair (princ-to-string (car pair)))))
  (let ((time (lt:now)))
    (bt:with-lock-held (*log-lock*)
      (with-open-file (s *logfile* :direction :output :if-exists :append :if-does-not-exist :create)
        (format s #?'{"when": "~A", "unix-time": ~D.~D, "remote": "~A", "message": ~A, "response": ~A, ~
                      "past-activations": ~A, "future-activations": ~A}~%'
                time (lt:timestamp-to-unix time) (round (lt:nsec-of time) 1000)
                (remote-host) message response
                (js:encode-json-to-string (first activations))
                (js:encode-json-to-string (second activations)))
        (force-output s)))))            ; probably redundant?

(defun receive-udp (buffer)
  (vom:debug1 "Received bytes: ~S" buffer)
  (let ((msg (string-trim #?" \n\r"
                          (bb:octets-to-string buffer :encoding :utf-8))))
    (vom:debug "Received: ~S" msg)
    (let (result activations)
      (handler-case
          (let* ((js:*json-identifier-name-to-lisp* #'js:simplified-camel-case-to-lisp)
                 (*read-default-float-format* 'double-float)
                 (json (with-input-from-string (s msg)
                         (js:decode-json-strict s))))
            (vom:debug1 "Decoded message: ~S" json)
            (let ((task (cdr (assoc :task json))))
              (unless (equalp task *current-task*)
                (setf *current-task* task)
                (let ((*time* (- *time* 1)))
                  (dolist (chunk-desc (cdr (assoc task *initial-data* :test #'equalp)))
                    (learn (iter (for (key val) :in chunk-desc)
                                 (collect (list key (and val (intern val))))))))))
            (multiple-value-setq (result activations) (run-model json)))
        (error (e)
          (vom:error "Error handling message ~A: ~A (~:*~S)" msg e)
          (setf result `((:error . ,(string (type-of e)))
                         (:message . ,msg)
                         (:description . ,(format nil "~A" e))
                         (:backtrace . ,(tb:print-backtrace e :output nil))))))
      (vom:debug1 "Result: ~S" result)
      (let ((response (js:encode-json-to-string result)))
        ;; seems a shame I can't figure out how to get CL-JSON to do it this way
        (setf response (re:regex-replace-all "\":null," response "\":{},"))
        (vom:debug "Replying: ~S" response)
        (write-log msg response activations)
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
