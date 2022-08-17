#|

ACT-UP 1.2
(c) Christian Lebiere, CMU

API:

- learn (<name> | (<content>))
   learn memory specified ny name or content
   if already exists, reinforces/rehearses it
   <name> is any symbol identifier
   <content> is a set of <attribute> <value> pairs
   <attribute> is any symbol identifier
   <value> is any value including numbers, strings, and symbol identifiers

- retrieve (<pattern>)
  retrieves most active pattern from memory
  <pattern> to be matched to (partial) <content>

- attribute (<memory> <attribute>)
  returns the value of <attribute> in <memory>

- actr-time ({<number>})
  increments time by <value> if <number> is included
  returns the value of time if <number> is not included

- similarity (<namei> <namej> {<number>})
  defines similarity between <namei> and <namej> if <number> is supplied
  returns similarity between <namei> and <namej> if <number> is not supplied

- parameter (<name> {<number>})
  sets parameter <name> to <number> if <number> included
  returns value of parameter <name> if number is not included
  acceptable values for parameter <name> include :bll (*decay*), :mp (*mismatch*), :ans (*noise*), :rt (*threshold*)

Todo:

- blend

Log:

- Replaced cons with list in building pairs of values in partial-match and blend
- Add value parameter of nil when calling similarity from partial-match and blend
- Checked when a similarity is not defined - return new *max-dif* parameter
- Added *similarity-hook-function* and call it in similarity when defined

6/21/19

- added special case testing for identity in similarity

V1.1

8/6/19

- Generalized blending for discrete values

12/25/19

- Reset *time* to 0.0 in init-memory
- Add *max-sim* as maximum similarity

3/5/20

- modify activation function to return 0 if *decay* rate is nil
- modify parameter function to allow setting parameter values to nil

V1.2

8/17/20

- add square function
- add blend-general function that uses similarities among output values
- fix: similarities hash table use #'equal rather than #'eql

2/12/21

- fix bug in learn to pass memory argument to create-chunk

4/26/21

- blend-vote returns third argument the alist of all values and probs and fourth argument the weighted sum

10/21/21

- in activation, move noise computation outside of decay when clause
- add option to set *noise* to nil and disable noise in that case

3/10/22

- fix bug in partial-match to keep best-activation (Pete Pirolli)

7/21/22

- fix bug in learn that creates a double reference for new chunks

Todo:

- more efficient perfect-match (used for merging in learn) using hashing

- consistency on keyword-vs-optional arguments

- consistency in passing memory argument around (actually, full structure with parameters, similarities, etc)

- add parameter initialization

- cache activations for a given time stamp

|#

;;; architectural parameters

(defvar *time* 0.0)

(defun get-time ()
  *time*)

(defun inc-time (inc)
  (incf *time* inc))

(defun actr-time (&optional (inc nil))
  (if inc (inc-time inc) (get-time)))

(defparameter *decay* 0.5)

(defparameter *optimized-learning* t)

(defparameter *mismatch* 1.0)

(defparameter *max-sim* 0.0)

(defparameter *max-dif* -1.0)

(defparameter *noise* 0.25)

(defparameter *threshold* -10.0)

(defparameter *temperature* 1.0)

(defparameter *similarity-hook-function* nil)

(defparameter *verbose* nil)

(defun parameter (name &optional (value :lookup))
  (if (eq value :lookup)
      (case name
        (:bll *decay*)
        (:ol *optimized-learning*)
        (:mp *mismatch*)
        (:ms *max-sim*)
        (:md *max-dif*)
        (:ans *noise*)
        (:rt *threshold*)
        (:tmp *temperature*)
        (:v *verbose*)
        (t (error "Unknown parameter ~S" name)))
      (case name
        (:bll (setf *decay* value))
        (:ol (setf *optimized-learning* value))
        (:mp (setf *mismatch* value))
        (:ms (setf *max-sim* value))
        (:md (setf *max-dif* value))
        (:ans (setf *noise* value))
        (:rt (setf *threshold* value))
        (:tmp (setf *temperature* value))
        (:v (setf *verbose* value))
        (t (error "Unknown parameter ~S" name)))
      ))

;;; memory structure
;;; name is separate but type (optional) is part of content with corresponding attribute names
(defstruct chunk
  (name nil)
  (content nil)
  (creation nil)
  (references nil))

(defparameter *memory* nil)

(defun init-memory ()
  (setf *time* 0.0)
  (setf *memory* (make-hash-table :test #'eql)))

(defun create-chunk (description &optional (memory *memory*) (trace *verbose*))
  (let ((name (if (symbolp description) description (gentemp "MEMORY")))
        (content (if (symbolp description) nil description)))
    (when trace (format t "Creating Chunk ~A Content ~S.~%" name content))
    (setf (gethash name memory) (make-chunk :name name :content content :creation (get-time)
                                            :references (if *optimized-learning* 1 (list (get-time)))))))

(defun dump-memory (&optional (memory *memory*))
  (let ((contents nil))
    (maphash #'(lambda (name chunk) (push (list name chunk) contents)) memory)
    contents))

(defun attribute (chunk attribute)
  (when chunk (second (assoc attribute (chunk-content chunk)))))

;;; similarities

(defstruct similarity
  (from nil)
  (to nil)
  (value nil))

(defparameter *similarities* nil)

(defun init-similarities ()
  (setf *similarities* (make-hash-table :test #'equal)))

(defun similarity-name (from to)
  (format nil "SIM-~A-~A" from to))

(defun create-similarity (from to value &optional (similarities *similarities*))
  (let ((name (similarity-name from to)))
    (setf (gethash name similarities) (make-similarity :from from :to to :value value))))

(defun similarity (from to &optional (value nil) (similarities *similarities*))
  (if value (create-similarity from to value similarities)
      (or
       (when *similarity-hook-function*                 ; if defined, call it
         (funcall *similarity-hook-function* from to))  ; and if it returns non-nil use that
       (when (eq from to) *max-sim*) ;;; FIX: add identity case
       (let ((similarity (gethash (similarity-name from to) similarities)))
         (if similarity (similarity-value similarity) *max-dif*)))))

;;; implement ACT-R approximation equation and exact one
(defun add-reference (chunk)
  (if *optimized-learning*
      (incf (chunk-references chunk))
      (push (get-time) (chunk-references chunk))))

(defun noise (s)
  "Approximates a sample from a normal distribution with mean zero and
   the given s-value (/ (sqrt (* 3.0 variance)) 3.1416)."
  ;; Need to test bound because of short-float lack of precision
  (let ((p (max 0.0001 (min (random 1.0) 0.9999))))
    (* s (log (/ (- 1.0 p) p)))))

(defun square (x)
  (* x x))

;;; just base-level activation for now; added noise now
(defun activation (chunk &optional (trace *verbose*))
  "Computes activation according to power law of practice and recency (unless *decay* is nil)."
  (let ((activation 0.0)
        (time (get-time)))
    (when *decay*
      (if *optimized-learning*
          (setf activation (/ (* (chunk-references chunk) (expt (- time (chunk-creation chunk)) (- *decay*)))
                              (- 1.0 *decay*)))
          (dolist (reference (chunk-references chunk))
            (incf activation (expt (- time reference) (- *decay*)))))
      (setf activation (log activation)))
    (when *noise* (incf activation (noise *noise*)))
    (when trace (format t "Calculating Chunk ~A Activation ~6,3F.~%" (chunk-name chunk) activation))
    activation))

(defun partial-match (conditions &optional (memory *memory*) (similarities *similarities*) (trace *verbose*))
  "Returns memory best matching specified conditions."
  (let ((best-chunk nil)
        (best-activation most-negative-short-float))
    (maphash #'(lambda (name chunk)
                 (let ((slot-match nil)
                       (content (chunk-content chunk))
                       (pairs nil))
                   (dolist (condition conditions (setf slot-match t))
                     (let ((slot-value (assoc (first condition) content)))
                       (if slot-value (push (list (second condition) (second slot-value)) pairs)
                           (return))))
                   (when slot-match
                     (let ((activation (activation chunk)))
                       (dolist (pair pairs)
                         (incf activation (* *mismatch* (similarity (first pair) (second pair) nil similarities))))
                       (when trace (format t "Calculating Chunk ~A Match Score ~6,3F.~%" name content))
                       (when (>= activation best-activation)
                         (setf best-activation activation) ;;; bug fix reported by Pete Pirolli (3/10/22)
                         (setf best-chunk chunk))))))
             memory)
    (values best-chunk best-activation)))

;;; Simplified version of blending, adapted from partial match function
(defun blend (conditions outcome &optional (memory *memory*) (similarities *similarities*) (trace *verbose*))
  "Returns blend best matching specified conditions as weighted average of slot outcome."
  ;;; partial match the conditions against each memory and blend the outcome slot (assuming numerical)
  ;;; weighted-sum is the sum over each chunk i of e^(Ai/t) where Ai is activation of chunk i
  ;;; weight-average is the sum over each chunk i of e^(Ai/t)*Oi where Oi is content of outcome slot of chunk i
  ;;; blended value of slot outcome is weighted-average/weighted-sum
  (let ((weighted-average 0.0)
        (weighted-sum 0.0))
    (maphash #'(lambda (name chunk)
                 (let ((slot-match nil)
                       (content (chunk-content chunk))
                       (pairs nil))
                   (dolist (condition conditions (setf slot-match t))
                     (let ((slot-value (assoc (first condition) content)))
                       (if slot-value (push (list (second condition) (second slot-value)) pairs)
                           (return))))
                   (when slot-match
                     (let ((activation (activation chunk)))
                       (dolist (pair pairs)
                         (incf activation (* *mismatch* (similarity (first pair) (second pair) nil similarities))))
                       (when trace (format t "Calculating Chunk ~A Match Score ~6,3F.~%" name activation)) ;;; FIX: second argument
                       (let ((weight (exp (/ activation *temperature*))))
                         (incf weighted-average (* weight (second (assoc outcome content))))
                         (incf weighted-sum weight))))))
             memory)
    (/ weighted-average weighted-sum)))

;;; Symbolic version of blending, voting for various discrete symbolic options
(defun blend-vote (conditions outcome &optional (memory *memory*) (similarities *similarities*) (trace *verbose*))
  "Returns blend best matching specified conditions as weighted average of slot outcome."
  ;;; partial match the conditions against each memory and blend the outcome slot (assuming numerical)
  ;;; weighted-sum is the sum over each chunk i of e^(Ai/t) where Ai is activation of chunk i
  ;;; weight-average is the sum over each chunk i of e^(Ai/t)*Oi where Oi is content of outcome slot of chunk i
  ;;; blended value of slot outcome is weighted-average/weighted-sum
  (let ((weighted-list nil)
        (weighted-sum 0.0))
    (maphash #'(lambda (name chunk)
                 (let ((slot-match nil)
                       (content (chunk-content chunk))
                       (pairs nil))
                   (dolist (condition conditions (setf slot-match t))
                     (let ((slot-value (assoc (first condition) content)))
                       (if slot-value (push (list (second condition) (second slot-value)) pairs)
                           (return))))
                   (when slot-match
                     (let ((activation (activation chunk)))
                       (dolist (pair pairs)
                         (incf activation (* *mismatch* (similarity (first pair) (second pair) nil similarities))))
                       (when trace (format t "Calculating Chunk ~A Match Score ~6,3F.~%" name activation)) ;;; FIX: second argument
                       (let* ((weight (exp (/ activation *temperature*)))
                              (value (second (assoc outcome content)))
                              (current (assoc value weighted-list)))
                         (if current (incf (second current) weight)
                             (push (list value weight) weighted-list))                         
;;;                         (incf weighted-average (* weight (second (assoc outcome content))))
                         (incf weighted-sum weight))))))
             memory)
    (setf weighted-list (sort weighted-list #'> :key #'second))
    (values (first (first weighted-list)) (/ (second (first weighted-list)) weighted-sum) weighted-list weighted-sum)
;;;    (/ weighted-average weighted-sum)
    ))

;;; Fully general version of blending, minimizing error for all possible values (appearing in chunks)
(defun blend-general (conditions outcome &optional (memory *memory*) (similarities *similarities*) (trace *verbose*))
  "Returns blend best matching specified conditions as minimizing weighted average of slot outcome (dis)similarities."
  ;;; partial match the conditions against each memory and blend the outcome slot (using similarities)
  ;;; weighted-sum is the sum over each chunk i of e^(Ai/t) where Ai is activation of chunk i
  ;;; weighted-list is the list of values V with the sum of e^(Ai/t)*Sim(Vi,V)^2 where Vi is content of outcome slot of chunk i
  ;;; blended value of slot outcome is the one with smallest error in weighted-list
  (let ((weighted-list nil)
        (weighted-sum 0.0))
    ;;; Collect all possible output values
    (maphash #'(lambda (name chunk)
                 (let ((slot-value (assoc outcome (chunk-content chunk))))
                   (when slot-value
                     (when trace (format t "Collecting Chunk ~A Outcome ~S Value ~S.~%" name outcome (second slot-value)))
                     (unless (assoc (second slot-value) weighted-list :test #'equal) ;;; unless already existing
                       (push (list (second slot-value) 0.0) weighted-list))))) ;;; initialize that slot value error
             memory)
    (maphash #'(lambda (name chunk)
                 (let ((slot-match nil)
                       (content (chunk-content chunk))
                       (pairs nil))
                   (dolist (condition conditions (setf slot-match t))
                     (let ((slot-value (assoc (first condition) content)))
                       (if slot-value (push (list (second condition) (second slot-value)) pairs)
                           (return))))
                   (when slot-match
                     (let ((activation (activation chunk)))
                       (dolist (pair pairs)
                         (incf activation (* *mismatch* (similarity (first pair) (second pair) nil similarities))))
                       (when trace (format t "Calculating Chunk ~A Match Score ~6,3F.~%" name activation))
                       (let* ((weight (exp (/ activation *temperature*)))
                              (value (second (assoc outcome content))))
;;;                              (current (assoc value weighted-list)))
                         (dolist (current weighted-list) ;;; iterate over all possible values
;;;                         (if current                  ;;; all values have been initialized
                           (incf (second current) (* weight (square (similarity value (first current))))) ;;; add weighted square similarity error
;;;                             (push (list value weight) weighted-list))                         
                           (incf weighted-sum weight)))))))
             memory)
    (setf weighted-list (sort weighted-list #'< :key #'second)) ;;; sort by decreasing error
    (values (first (first weighted-list)) (/ (second (first weighted-list)) weighted-sum) weighted-list)
    ))


(defun exact-match (conditions &optional (memory *memory*) (trace *verbose*))
  "Returns most active memory exactly matching specified conditions."
  (let ((best-chunk nil)
        (best-activation *threshold*))
    (maphash #'(lambda (name chunk) (when (subsetp conditions (chunk-content chunk) :test #'equal)
                                      (let ((activation (activation chunk)))
                                        (when trace (format t "Chunk ~A exactly matches to Conditions ~S with activation ~6,3F.~%"
                                                            name conditions activation))
                                        (when (>= activation best-activation)
                                          (setf best-chunk chunk)
                                          (setf best-activation activation))))) memory)
    (values best-chunk best-activation)))

(defun perfect-match (conditions &optional (memory *memory*) (trace *verbose*))
  "Returns chunk perfectly matching specified conditions."
  (let ((best-chunk nil)
        (conds (length conditions)))
    (maphash #'(lambda (name chunk) (when (and (subsetp conditions (chunk-content chunk) :test #'equal)
                                               (= conds (length (chunk-content chunk))))
                                      (when trace (format t "Chunk ~A pefectly matches to Conditions ~S.~%" name conditions))
                                      (setf best-chunk chunk))) memory)
    best-chunk))

(defun retrieve (conditions &key (mode 'exact) (memory *memory*) (trace *verbose*))
  (case mode 
    (exact (exact-match conditions memory trace))
    (t (partial-match conditions memory trace))))

(defun learn (description &key (memory *memory*))
  "Learns or rehearses chnk with description consisting of name or content (attribute value pairs)."
  (let ((chunk (if (symbolp description) (gethash description memory) (perfect-match description memory))))
    (if chunk
        (add-reference chunk)
        (setf chunk (create-chunk description memory)))
    chunk))

#|
;;; Testing lists, a-lists and hash tables for memory
;;; Hash table considerably faster at retrieval with little space or time overhead at creation

(defparameter *names* nil)

(defparameter *results* nil)

(defun set-memory (n type)
  (let ((names nil))
    (setf *memory* nil)
    (dotimes (i n)
      (let* ((name (gentemp "MEMORY"))
             (memory (make-memory :name name)))
        (push name names)
        (case type
          (list (push memory *memory*))
          (alist (push (cons name memory) *memory*))
          (hash (when (null *memory*) (setf *memory* (make-hash-table :test #'eq)))
                (setf (gethash name *memory*) memory))
          (t (error "unknown memory type ~S" type)))))
    (setf *names* names)
    ()))

(defun test-find (&optional (names *names*) (memory *memory*))
  (let ((results nil))
    (dolist (name names)
      (push (find name memory :test #'eq :key #'memory-name) results))
    (setf *results* results)
    ()))

(defun test-assoc (&optional (names *names*) (memory *memory*))
  (let ((results nil))
    (dolist (name names)
      (push (assoc name memory :test #'eq) results))
    (setf *results* results)
    ()))

(defun test-hash (&optional (names *names*) (memory *memory*)) 
  (let ((results nil))
    (dolist (name names)
      (push (gethash name memory) results))
    (setf *results* results)
    ()))

? (time (set-memory 1000 'list))
(SET-MEMORY 1000 'LIST)
took 6,498 microseconds (0.006498 seconds) to run.
During that period, and with 2 available CPU cores,
     9,179 microseconds (0.009179 seconds) were spent in user mode
     1,097 microseconds (0.001097 seconds) were spent in system mode
 346,256 bytes of memory allocated.
 4 minor page faults, 1 major page faults, 0 swaps.
NIL
? (time (test-find))
(TEST-FIND)
took 12,961 microseconds (0.012961 seconds) to run.
During that period, and with 2 available CPU cores,
     17,793 microseconds (0.017793 seconds) were spent in user mode
        415 microseconds (0.000415 seconds) were spent in system mode
 16,000 bytes of memory allocated.
 1 minor page faults, 0 major page faults, 0 swaps.
NIL
? (length *results*)
1000
? (time (set-memory 10000 'list))
(SET-MEMORY 10000 'LIST)
took 50,365 microseconds (0.050365 seconds) to run.
      5,174 microseconds (0.005174 seconds, 10.27%) of which was spent in GC.
During that period, and with 2 available CPU cores,
     52,841 microseconds (0.052841 seconds) were spent in user mode
      2,143 microseconds (0.002143 seconds) were spent in system mode
 3,361,520 bytes of memory allocated.
 98 minor page faults, 0 major page faults, 0 swaps.
NIL
? (time (test-find))
(TEST-FIND)
took 1,094,413 microseconds (1.094413 seconds) to run.
During that period, and with 2 available CPU cores,
     1,104,176 microseconds (1.104176 seconds) were spent in user mode
         2,582 microseconds (0.002582 seconds) were spent in system mode
 160,000 bytes of memory allocated.
 61 minor page faults, 0 major page faults, 0 swaps.
NIL
? (time (test-find))
(TEST-FIND)
took 1,115,916 microseconds (1.115916 seconds) to run.
During that period, and with 2 available CPU cores,
     1,173,506 microseconds (1.173506 seconds) were spent in user mode
        13,251 microseconds (0.013251 seconds) were spent in system mode
 160,000 bytes of memory allocated.
 1,103 minor page faults, 3 major page faults, 0 swaps.
NIL
? (length *results*)
10000
? (time (set-memory 10000 'alist))
(SET-MEMORY 10000 'ALIST)
took 62,792 microseconds (0.062792 seconds) to run.
      6,842 microseconds (0.006842 seconds, 10.90%) of which was spent in GC.
During that period, and with 2 available CPU cores,
     63,655 microseconds (0.063655 seconds) were spent in user mode
      2,717 microseconds (0.002717 seconds) were spent in system mode
 3,878,672 bytes of memory allocated.
 261 minor page faults, 0 major page faults, 0 swaps.
NIL
? (time (test-assoc))
(TEST-ASSOC)
took 278,866 microseconds (0.278866 seconds) to run.
         285 microseconds (0.000285 seconds, 0.10%) of which was spent in GC.
During that period, and with 2 available CPU cores,
     293,644 microseconds (0.293644 seconds) were spent in user mode
       1,099 microseconds (0.001099 seconds) were spent in system mode
 160,000 bytes of memory allocated.
 2 minor page faults, 0 major page faults, 0 swaps.
NIL
? (time (test-assoc))
(TEST-ASSOC)
took 278,293 microseconds (0.278293 seconds) to run.
During that period, and with 2 available CPU cores,
     294,182 microseconds (0.294182 seconds) were spent in user mode
         794 microseconds (0.000794 seconds) were spent in system mode
 160,000 bytes of memory allocated.
 1 minor page faults, 0 major page faults, 0 swaps.
NIL
? (length *results*)
10000
? (time (set-memory 10000 'hash))
(SET-MEMORY 10000 'HASH)
took 102,248 microseconds (0.102248 seconds) to run.
       7,493 microseconds (0.007493 seconds, 7.33%) of which was spent in GC.
During that period, and with 2 available CPU cores,
     113,685 microseconds (0.113685 seconds) were spent in user mode
       4,363 microseconds (0.004363 seconds) were spent in system mode
 4,651,120 bytes of memory allocated.
 412 minor page faults, 0 major page faults, 0 swaps.
NIL
? (time (test-hash))
(TEST-HASH)
took 1,423 microseconds (0.001423 seconds) to run.
During that period, and with 2 available CPU cores,
     2,393 microseconds (0.002393 seconds) were spent in user mode
       263 microseconds (0.000263 seconds) were spent in system mode
 160,000 bytes of memory allocated.
 31 minor page faults, 0 major page faults, 0 swaps.
NIL
? (time (test-hash))
(TEST-HASH)
took 1,454 microseconds (0.001454 seconds) to run.
During that period, and with 2 available CPU cores,
     2,613 microseconds (0.002613 seconds) were spent in user mode
       218 microseconds (0.000218 seconds) were spent in system mode
 160,000 bytes of memory allocated.
 1 minor page faults, 0 major page faults, 0 swaps.
NIL
? (length *results*)
10000
? (time (set-memory 100000 'list))
(SET-MEMORY 100000 'LIST)
took 1,254,637 microseconds (1.254637 seconds) to run.
       221,935 microseconds (0.221935 seconds, 17.69%) of which was spent in GC.
During that period, and with 2 available CPU cores,
     1,201,799 microseconds (1.201799 seconds) were spent in user mode
        47,832 microseconds (0.047832 seconds) were spent in system mode
 43,085,440 bytes of memory allocated.
 8,305 minor page faults, 0 major page faults, 0 swaps.
NIL
? (time (test-find))
(TEST-FIND)
took 285,481,856 microseconds (285.481840 seconds) to run.
           3,198 microseconds (  0.003198 seconds, 0.00%) of which was spent in GC.
During that period, and with 2 available CPU cores,
     285,272,144 microseconds (285.272130 seconds) were spent in user mode
         290,283 microseconds (  0.290283 seconds) were spent in system mode
 1,600,000 bytes of memory allocated.
 1,527 minor page faults, 0 major page faults, 0 swaps.
NIL
? (time (set-memory 100000 'alist))
(SET-MEMORY 100000 'ALIST)
took 1,532,475 microseconds (1.532475 seconds) to run.
       269,416 microseconds (0.269416 seconds, 17.58%) of which was spent in GC.
During that period, and with 2 available CPU cores,
     1,488,579 microseconds (1.488579 seconds) were spent in user mode
        36,788 microseconds (0.036788 seconds) were spent in system mode
 46,399,408 bytes of memory allocated.
 5,325 minor page faults, 0 major page faults, 0 swaps.
NIL
? (time (test-assoc))
(TEST-ASSOC)
took 263,612,475 microseconds (263.612500 seconds) to run.
           1,911 microseconds (  0.001911 seconds, 0.00%) of which was spent in GC.
During that period, and with 2 available CPU cores,
     262,889,333 microseconds (262.889300 seconds) were spent in user mode
         279,068 microseconds (  0.279068 seconds) were spent in system mode
 1,600,000 bytes of memory allocated.
 1,513 minor page faults, 1 major page faults, 0 swaps.
NIL
? (time (set-memory 100000 'hash))
(SET-MEMORY 100000 'HASH)
took 1,707,298 microseconds (1.707298 seconds) to run.
       353,665 microseconds (0.353665 seconds, 20.71%) of which was spent in GC.
During that period, and with 2 available CPU cores,
     1,667,547 microseconds (1.667547 seconds) were spent in user mode
        38,331 microseconds (0.038331 seconds) were spent in system mode
 48,924,800 bytes of memory allocated.
 5,826 minor page faults, 0 major page faults, 0 swaps.
NIL
? (time (test-hash))
(TEST-HASH)
took 25,826 microseconds (0.025826 seconds) to run.
      4,300 microseconds (0.004300 seconds, 16.65%) of which was spent in GC.
During that period, and with 2 available CPU cores,
     40,193 microseconds (0.040193 seconds) were spent in user mode
      1,420 microseconds (0.001420 seconds) were spent in system mode
 1,600,000 bytes of memory allocated.
 1 minor page faults, 0 major page faults, 0 swaps.
NIL
? 

;;; Testing compilation of internal maphash function
;;; No effect of speed so inline function just fine

(defun test-maphash-inline (&optional (values 100000) (target 42))
  (let ((memory (init-memory))
        (chunks nil))
    (dotimes (value values)
      (create-chunk (list (list 'value value)) memory))
    (maphash #'(lambda (name chunk) (when (= (chunk-slot-value chunk 'value) target) (push name chunks))) memory)
    chunks))

(defun test-maphash-labels (&optional (values 100000) (target 42))
  (let ((memory (init-memory))
        (chunks nil))
    (labels ((screen-value (name chunk) (when (= (chunk-slot-value chunk 'value) target) (push name chunks))))
      (dotimes (value values)
        (create-chunk (list (list 'value value)) memory))
      (maphash #'screen-value memory))
    chunks))

(defparameter *chunks* nil)

(defparameter *target* nil)

(defun screen-value (name chunk)
  (when (= (chunk-slot-value chunk 'value) 42) (push name *chunks*)))

(defun test-maphash-outline (&optional (values 100000) (target 42))
  (let ((memory (init-memory)))
    (setf *chunks* nil)
    (setf *target* target)
    (dotimes (value values)
      (create-chunk (list (list 'value value)) memory))
    (maphash #'screen-value memory)
    *chunks*))

? (time (test-maphash-inline 100000))
(TEST-MAPHASH-INLINE 100000)
took 2,438,303 microseconds (2.438303 seconds) to run.
     1,162,622 microseconds (1.162622 seconds, 47.68%) of which was spent in GC.
During that period, and with 2 available CPU cores,
     1,537,793 microseconds (1.537793 seconds) were spent in user mode
       168,570 microseconds (0.168570 seconds) were spent in system mode
 54,834,608 bytes of memory allocated.
 13,899 minor page faults, 3,438 major page faults, 0 swaps.
(MEMORY13042)
? (time (test-maphash-inline 100000))
(TEST-MAPHASH-INLINE 100000)
took 1,751,079 microseconds (1.751079 seconds) to run.
       357,706 microseconds (0.357706 seconds, 20.43%) of which was spent in GC.
During that period, and with 2 available CPU cores,
     1,714,506 microseconds (1.714506 seconds) were spent in user mode
        35,904 microseconds (0.035904 seconds) were spent in system mode
 55,561,744 bytes of memory allocated.
 4,678 minor page faults, 0 major page faults, 0 swaps.
(MEMORY113042)
? (time (test-maphash-inline 100000))
(TEST-MAPHASH-INLINE 100000)
took 1,793,059 microseconds (1.793059 seconds) to run.
       367,136 microseconds (0.367136 seconds, 20.48%) of which was spent in GC.
During that period, and with 2 available CPU cores,
     1,754,819 microseconds (1.754819 seconds) were spent in user mode
        33,830 microseconds (0.033830 seconds) were spent in system mode
 54,724,912 bytes of memory allocated.
 3,234 minor page faults, 0 major page faults, 0 swaps.
(MEMORY213042)
? (time (test-maphash-labels 100000))
(TEST-MAPHASH-LABELS 100000)
took 2,442,602 microseconds (2.442602 seconds) to run.
       653,100 microseconds (0.653100 seconds, 26.74%) of which was spent in GC.
During that period, and with 2 available CPU cores,
     2,401,545 microseconds (2.401545 seconds) were spent in user mode
        34,714 microseconds (0.034714 seconds) were spent in system mode
 58,246,112 bytes of memory allocated.
 3,451 minor page faults, 0 major page faults, 0 swaps.
(MEMORY413042)
? (time (test-maphash-labels 100000))
(TEST-MAPHASH-LABELS 100000)
took 2,162,946 microseconds (2.162946 seconds) to run.
       541,468 microseconds (0.541468 seconds, 25.03%) of which was spent in GC.
During that period, and with 2 available CPU cores,
     2,113,832 microseconds (2.113832 seconds) were spent in user mode
        46,338 microseconds (0.046338 seconds) were spent in system mode
 55,923,280 bytes of memory allocated.
 5,273 minor page faults, 0 major page faults, 0 swaps.
(MEMORY513042)
? (time (test-maphash-labels 100000))
(TEST-MAPHASH-LABELS 100000)
took 1,829,628 microseconds (1.829628 seconds) to run.
       537,906 microseconds (0.537906 seconds, 29.40%) of which was spent in GC.
During that period, and with 2 available CPU cores,
     1,854,499 microseconds (1.854499 seconds) were spent in user mode
        38,333 microseconds (0.038333 seconds) were spent in system mode
 50,967,856 bytes of memory allocated.
 4,165 minor page faults, 1 major page faults, 0 swaps.
(MEMORY613042)
? (time (test-maphash-labels 100000))
(TEST-MAPHASH-LABELS 100000)
took 2,430,531 microseconds (2.430531 seconds) to run.
       545,322 microseconds (0.545322 seconds, 22.44%) of which was spent in GC.
During that period, and with 2 available CPU cores,
     2,410,200 microseconds (2.410200 seconds) were spent in user mode
        31,451 microseconds (0.031451 seconds) were spent in system mode
 59,489,312 bytes of memory allocated.
 2,056 minor page faults, 0 major page faults, 0 swaps.
(MEMORY713042)
? (time (test-maphash-outline 100000))
(TEST-MAPHASH-OUTLINE 100000)
took 2,356,631 microseconds (2.356631 seconds) to run.
       749,384 microseconds (0.749384 seconds, 31.80%) of which was spent in GC.
During that period, and with 2 available CPU cores,
     2,451,682 microseconds (2.451682 seconds) were spent in user mode
        58,129 microseconds (0.058129 seconds) were spent in system mode
 58,871,232 bytes of memory allocated.
 8,046 minor page faults, 0 major page faults, 0 swaps.
(MEMORY1113042)
? (time (test-maphash-outline 100000))
(TEST-MAPHASH-OUTLINE 100000)
took 2,449,462 microseconds (2.449462 seconds) to run.
       802,060 microseconds (0.802060 seconds, 32.74%) of which was spent in GC.
During that period, and with 2 available CPU cores,
     2,422,398 microseconds (2.422398 seconds) were spent in user mode
        27,595 microseconds (0.027595 seconds) were spent in system mode
 59,931,504 bytes of memory allocated.
 1,866 minor page faults, 0 major page faults, 0 swaps.
(MEMORY1213042)
? (time (test-maphash-outline 100000))
(TEST-MAPHASH-OUTLINE 100000)
took 2,572,552 microseconds (2.572552 seconds) to run.
       794,227 microseconds (0.794227 seconds, 30.87%) of which was spent in GC.
During that period, and with 2 available CPU cores,
     2,555,956 microseconds (2.555956 seconds) were spent in user mode
        42,239 microseconds (0.042239 seconds) were spent in system mode
 61,091,139 bytes of memory allocated.
 5,120 minor page faults, 0 major page faults, 0 swaps.
(MEMORY1313042)

|#
