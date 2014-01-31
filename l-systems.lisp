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


(defparameter *hilbert* (make-instance 'l-system
				       :axiom "a"
				       :alphabet '(#\a #\b)
				       :productions (list
						     (make-instance 'production :predecessor #\a :successor "-bf+afa+fb-")
						     (make-instance 'production :predecessor #\b :successor "+af-bfb-fa+"))))


(defclass l-system-drawer ()
  ((input :acccessor input :initarg :input)
   (initial-direction :accessor initial-direction :initarg :initial-direction)
   (origin :accessor :origin :initarg :origin)))
