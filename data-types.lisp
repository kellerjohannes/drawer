(in-package :drawer)


'(
  (:type :scalar :id :a :value 15)
  (:type :scalar :id :b :value 17)
  (:type :point :id :aa :x-coord (rel :a) :y-coord (rel :b))
  )




(defparameter *nodes* nil)

(defun get-node (id)
  (find id *nodes* :key (lambda (item) (getf item :id))))

(defun add-node (new-node)
  (let ((new-id (getf new-node :id)))
    (if (get-node new-id)
        (error "Node with id ~s already exists" new-id)
        (push new-node *nodes*))))

(defmacro make-scalar (id value)
  `(add-node '(:type :scalar :id ,id :value ,value)))

(defmacro make-point (id x y)
  `(add-node '(:type :point :id ,id :x ,x :y ,y)))

(defmacro make-line (id origin destination)
  `(add-node '(:type :line :id ,id :origin ,origin :destination ,destination)))



(defun rel (id)
  ())



(defun make-scalar (value)
  (lambda () value))

(defun make-point (x-coord y-coord)
  (lambda ()
    ()))

(defun get-x-coord (point)
  (car point))

(defun get-y-coord (point)
  (cdr point))


(defun make-line (point-a point-b)
  (cons point-a point-b))

(defun get-point-a (line)
  (car line))

(defun get-point-b (line)
  (cdr line))


(defun make-circle (point-center radius)
  (cons point-center radius))

(defun get-circle-center (circle)
  (car circle))

(defun get-circle-radius (circle)
  (cdr circle))


(defun make-text (text-string point-position)
  (cons text-string point-position))

(defun get-text-string (text)
  (car text))

(defun get-point-position (text)
  (cdr text))
