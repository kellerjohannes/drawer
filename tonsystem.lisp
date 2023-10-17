(in-package :drawer)

(defclass tonsystem ()
  ())

(defclass tonnetz-node ()
  ((label :initarg :label :initform nil :accessor label)))

(defmethod print-object ((object tonnetz-node) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "label: ~a" (label object))))



(defclass tonnetz (tonsystem)
  ((dimensions :initform '(1 1) :initarg :dimensions :accessor dimensions)
   (offsets :initform '(0 0) :initarg :offsets :accessor offsets)
   (nodes :accessor nodes)
   (connection-list :accessor connection-list)))

(defmethod initialize-instance :after ((system tonnetz) &key)
  (setf (nodes system) (make-array (dimensions system)))
  (loop for i from 0 to (1- (reduce #'* (dimensions system)))
        do (setf (row-major-aref (nodes system) i) (make-instance 'tonnetz-node))))

(defmethod print-netz ((system tonnetz))
  (format t "~&~a" (nodes system)))

(defmethod make-tonnetz (dimension-list offset-list)
  (make-instance 'tonnetz :dimensions dimension-list :offsets offset-list))

(defun construct-subscripts (dimension-mask index)
  (let ((result (copy-list dimension-mask)))
    (setf (elt result (position nil result)) index)
    result))

(defmethod get-offset-subscripts ((system tonnetz) subscripts)
  (if (= (length subscripts) (length (offsets system)))
      (mapcar (lambda (index offset) (+ index offset)) subscripts (offsets system))
      (error "Provided subscripts ~a don't match the offsets of the system, ~a."
             subscripts (dimensions system))))

(defparameter *testnetz* (make-tonnetz '(6 4) '(2 2)))

(defmethod get-node ((system tonnetz) subscripts)
  (apply #'aref (nodes system) (get-offset-subscripts system subscripts)))

(defmethod init-row ((system tonnetz) dimension-mask start-position label-list)
  (loop for lbl in label-list
        for i from start-position
        do (setf (label (get-node system (construct-subscripts dimension-mask i)))
                 lbl)))




(let ((system (make-tonnetz '(6 4) '(2 2))))
  (init-row system '(nil 1) -1 '("D♭" "A♭" "E♭" "B♭"))
  (init-row system '(nil 0) -2 '("B♭" "F" "C" "G" "D" "A"))
  (init-row system '(nil -1) -2 '("D" "A" "E" "B" "F♯"))
  (init-row system '(nil -2) -2 '("F♯" "C♯" "G♯" "D♯")))
