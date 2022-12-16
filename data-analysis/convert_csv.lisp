(ql:quickload '(:cl-json))

(defun convert (user-id)
  (let ((json-filename (make-pathname :type "json" :name (format nil "~A-activation-data" user-id)))
	(cogload-filename (make-pathname :type "csv" :name (format nil "~A-model-data-2.0" user-id)))
	(past-activation-filename (make-pathname :type "csv" :name (format nil "~A-PastActivation-data-2.0" user-id)))
	(future-activation-filename (make-pathname :type "csv" :name (format nil "~A-FutureActivation-data-2.0" user-id)))
	(current-task "start")
	(future-list '())
	(match-any 0)
	(match-1 0)
	(match-2 0)
	(match-3 0)
	(tot-action-num 0)
	(task-action-num 0)
	(temperature 1.0)
	(thresh -2.0)
	)

    ;;open file to write cogload data to
    (with-open-file (str cogload-filename
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (format str "UserID,TotalActionNum,Time,Task,TaskActionNum,Address,Past_1,Past_2,Past_3,Past_4,Past_5,Future_1,Future_2,Future_3,Match_Any,Match_1,Match_2,Match_3,Past_CogLoad,Future_CogLoad~%"))
    
    ;;open file to write past activation data to
    (with-open-file (str past-activation-filename
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (format str "UserID,TotalActionNum,Time,Task,TaskActionNum,Address,ChunkSpec,Activation~%"))

    ;;open file to write future activation data to
    (with-open-file (str future-activation-filename
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (format str "UserID,TotalActionNum,Time,Task,TaskActionNum,Address,ChunkSpec,Activation~%"))
    
    ;;read in data file line by line
    (with-open-file (stream json-filename)
      (loop for line = (read-line stream nil)
	    while line
	    do
	       (let* ((json (with-input-from-string
				(s line)
			      (let ((*read-default-float-format* 'double-float)
				    (json:*json-symbols-package* nil)
				    (json:*identifier-name-to-key* (lambda (x) x))
				    (json:*json-identifier-name-to-lisp* (lambda (x) x)))
				(json:decode-json-strict s))))
		      (task (cdr (assoc "Task" (cdr (assoc "message" json :test #'equal)) :test #'equal)))
		      (address (cdr (assoc "ID" (cdr (assoc "message" json :test #'equal)) :test #'equal)))
		      (time (cdr (assoc "timestamp" (cdr (assoc "message" json :test #'equal)) :test #'equal)))
		      (past-1 (car (first (cdr (assoc "past" (cdr (assoc "response" json :test #'equal)) :test #'equal)))))
		      (past-2 (car (second (cdr (assoc "past" (cdr (assoc "response" json :test #'equal)) :test #'equal)))))
		      (past-3 (car (third (cdr (assoc "past" (cdr (assoc "response" json :test #'equal)) :test #'equal)))))
		      (past-4 (car (fourth (cdr (assoc "past" (cdr (assoc "response" json :test #'equal)) :test #'equal)))))
		      (past-5 (car (fifth (cdr (assoc "past" (cdr (assoc "response" json :test #'equal)) :test #'equal)))))
		      (future-1 (car (first (cdr (assoc "future" (cdr (assoc "response" json :test #'equal)) :test #'equal)))))
		      (future-2 (car (second (cdr (assoc "future" (cdr (assoc "response" json :test #'equal)) :test #'equal)))))
		      (future-3 (car (third (cdr (assoc "future" (cdr (assoc "response" json :test #'equal)) :test #'equal)))))
		      (past-activation-list (cdr (assoc "past-activations" json :test #'equal)))
		      (future-activation-list (cdr (assoc "future-activations" json :test #'equal)))
		      (past-cog-load 0)
		      (future-cog-load 0)
		      )
		 (if (not (equal current-task task))
		     (setf future-list '()))
		 (setf match-any (if (member address future-list :test #'equal) 1 0))
		 (setf match-1 (if (equal address (first future-list)) 1 0))
		 (setf match-2 (if (equal address (second future-list)) 1 0))
		 (setf match-3 (if (equal address (third future-list)) 1 0))
		 (setf future-list `(,future-1 ,future-2 ,future-3))
		 
		 ;;set current task number
		 (if (equal current-task task)
		     (incf task-action-num)
		     (setf task-action-num 1))
		 (setf current-task task)
		 (incf tot-action-num)
		 ;;(format t "~S~%" json)
		 (format t "Task: ~A (~17,6F)~%" task time)
		 
		 ;;loop through past-activations list, compute cog load, and write lines to file
		 (dolist (chunk-data past-activation-list)
		   (let ((chunk-spec (car chunk-data))
			 (activation (cdr chunk-data)))
		     (incf past-cog-load (exp (/ (- thresh activation) temperature)))
		     (with-open-file (str past-activation-filename
					  :direction :output
					  :if-exists :append
					  :if-does-not-exist :create)
		       (format str "~A,~A,~17,6F,~A,~A,~A,~A,~F~%"
			       user-id tot-action-num time task task-action-num address chunk-spec activation))))

		 ;;loop through future-activation list, compute cog load, and write lines to file
		 (dolist (chunk-data future-activation-list)
		   (let ((chunk-spec (car chunk-data))
			 (activation (cdr chunk-data)))
		     (incf future-cog-load (exp (/ (- thresh activation) temperature)))
		     (with-open-file (str future-activation-filename
					  :direction :output
					  :if-exists :append
					  :if-does-not-exist :create)
		       (format str "~A,~A,~17,6F,~A,~A,~A,~A,~F~%"
			       user-id tot-action-num time task task-action-num address chunk-spec activation))))		 
		 
		 ;;write cogload and click data to file
		 (with-open-file (str cogload-filename
				      :direction :output
				      :if-exists :append
				      :if-does-not-exist :create)
		   (format str "~A,~A,~17,6F,~A,~A,~A,~A,~A,~A,~A,~A,~A,~A,~A,~A,~A,~A,~A,~F,~F~%"
			   user-id tot-action-num time task task-action-num address
			   past-1 past-2 past-3 past-4 past-5 future-1 future-2 future-3
			   match-any match-1 match-2 match-3 past-cog-load future-cog-load))
		 )
	    )
      )
    )
  )
