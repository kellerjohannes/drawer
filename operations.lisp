(in-package :drawer)

(defgeneric copy-move (drawer-object point))

(defmethod copy-move ((obj point) (anchor point) (target point))
    (add obj (subtract target anchor)))

(defmethod copy-move ((obj line) (anchor point) (target point))
  (make-line (copy-move (origin obj) anchor target)
      (copy-move (destination obj) anchor target)))

(defmethod copy-move ((obj line-strip) (anchor point) (target point))
  (make-line-strip (loop for point in (point-list obj)
                         collect (copy-move point anchor target))))

(defmethod copy-move ((obj circle) (anchor point) (target point))
  (make-circle (copy-move (center obj) anchor target) (radius obj)))

(defmethod copy-move ((obj group) (anchor point) (target point))
  (make-group (loop for element in (content obj)
                    collect (copy-move element anchor target))))


(defmethod above ((origin point) (distance scalar))
  (make-point (x origin) (make-scalar (+ (value (y origin)) (value distance)))))

(defmethod below ((origin point) (distance scalar))
  (make-point (x origin) (make-scalar (- (value (y origin)) (value distance)))))

(defmethod right-of ((origin point) (distance scalar))
  (make-point (make-scalar (+ (value (x origin)) (value distance))) (y origin)))

(defmethod left-of ((origin point) (distance scalar))
  (make-point (make-scalar (- (value (x origin)) (value distance))) (y origin)))


(defmethod make-obj-array ((obj drawer-object) iterations (anchor point) (target point) (delta point))
  (let ((result (make-array iterations :element-type (type-of obj))))
    (loop for i from 0 to (1- iterations)
          do (setf (aref result i) (copy-move obj anchor (add target (scale delta i)))))
    result))
