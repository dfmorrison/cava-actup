(ql:quickload '(:cl-json))

(defun convert (user-id)
  (let ((json-filename (make-pathname :type "json" :name (format nil "~A_distance_data" user-id)))
	(csv-filename (make-pathname :type "csv" :name (format nil "~A_distance_data" user-id)))
	(current-task "start")
	(tot-action-num 0)
	(task-action-num 0))

    ;;open file to write distance data to
    (with-open-file (str csv-filename
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (format str "UserID,TotalActionNum,Time,Task,TaskActionNum,Address,Key,AssemblyDist,BlockDist,FunctionDist~%"))

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
		      (time (cdr (assoc "timestamp" json :test #'equal)))
		      (task (cdr (assoc "Task" json :test #'equal)))
		      (address (cdr (assoc "ClickedID" json :test #'equal)))
		      (key (cdr (assoc "KeyID" json :test #'equal)))
		      (assemblydist (cdr (assoc "AssemblyDist" json :test #'equal)))
		      (blockdist (cdr (assoc "BlockDist" json :test #'equal)))
		      (funcdist (cdr (assoc "FunctionDist" json :test #'equal))))
		 ;;set current task number
		 (if (equal current-task task)
		     (incf task-action-num)
		     (setf task-action-num 1))
		 (setf current-task task)
		 (incf tot-action-num)
		 ;;(format t "~S~%" json)
		 (format t "Task: ~A (~6,10$)~%" task time)		 
		 
		 ;;write distance data to file
		 (with-open-file (str csv-filename
				      :direction :output
				      :if-exists :append
				      :if-does-not-exist :create)
		   (format str "~A,~A,~6,10$,~A,~A,~A,~A,~A,~A,~A~%"
			   user-id tot-action-num time task task-action-num
			   address key assemblydist blockdist funcdist))))))
  )
