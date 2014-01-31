;;;; l-systems.lisp

(in-package #:l-systems)

;;; "l-systems" goes here. Hacks and glory await!

(defclass l-system ()
  ((axiom :initarg :axiom :accessor axiom)
   (alphabet :initarg :alphabet :accessor alphabet)
   (productions :initarg :productions :accessor productions)))

(defclass production ()
  ((predecessor :initarg :predecessor :accessor predecessor)
   (successor :initarg :successor :accessor successor)))

(defmethod l-system-step ((l-system l-system) state)
  (let ((applicable-productions (iter (for letter in-string state with-index index)
				      (collect (cons index (applicable-productions l-system state index))))))
    (iter (for (index . productions) in applicable-productions)
	  (collect (if productions
		       (apply-production l-system (select-applicable-production l-system productions state index) index state)
		       (subseq state index (1+ index)))))))

(defmethod applicable-productions ((l-system l-system) state index)
  (iter (for production in (productions l-system))
	(when (can-apply-production l-system production state index)
	  (collect production))))

(defmethod can-apply-production ((l-system l-system) (production production) state index)
  (declare (ignorable l-system))
  (char= (elt state index) (predecessor production)))

(defmethod select-applicable-production ((l-system l-system) productions state index)
  (first productions))

(defmethod apply-production ((l-system l-system) (production production) state index)
  (successor production))


(defun concat-strings (strings)
  (let ((result (make-array (reduce #'+ strings :key #'length) :element-type 'character :fill-pointer 0)))
   (iter (for string in strings)
	 (iter (for letter in-string string)
	       (vector-push letter result)))
   (coerce result 'string)))
(defmethod grow-l-system (l-system level)
  (let ((state (axiom l-system)))
    (dotimes (level level)
      (setf state (concat-strings (l-system-step l-system state))))
    state))

(defun find-symbols (axiom productions)
  (let ((letters (make-hash-table)))
    (iter (for letter in-string axiom)
	  (setf (gethash letter letters) t))
    (iter (for (pred succ) in productions)
	  (iter (for letter in-string (concatenate 'string pred succ))
		(setf (gethash letter letters) t)))
    (iter (for (key value) in-hashtable letters)
	  (collect key))))
(defun make-productions (productions)
  (iter (for (pred succ) in productions)
	(collect (make-instance 'production :predecessor (elt pred 0) :successor succ))))
(defun make-l-system (axiom productions)
  "axiom is a single character string, productions is a list of 2 element lists, first element is a source string, 2nd element a target string"
  (make-instance 'l-system
		 :axiom axiom
		 :alphabet (find-symbols axiom productions)
		 :productions (make-productions productions)))
(defun split-system ()
  (make-l-system "0" '(("0" "1[0]0")
		       ("1" "11"))))
(defun algae-system ()
  (make-l-system "a" '(("a" "ab")
		       ("b" "a"))))
(defun cantor-dust-system ()
  (make-l-system "a" '(("a" "aba")
		       ("b" "bbb"))))
(defun koch-curve-system ()
  (make-l-system "f" '(("f" "f+f-f-f+f"))))
(defun sierpinski-triangle-system ()
  (make-l-system "a" '(("a" "b-a-b")
		       ("b" "a+b+a"))))
(defun hilbert-system ()
    (make-l-system "a" '(("a" "-bf+afa+fb-")
			 ("b" "+af-bfb-fa+"))))

(defclass l-system-drawer ()
  ((input :accessor input :initarg :input)
   (initial-direction :accessor initial-direction :initarg :initial-direction)
   (origin :accessor :origin :initarg :origin)))
