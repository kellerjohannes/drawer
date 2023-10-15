(in-package :drawer)



(defun pt (x-coord y-coord)
  (make-point (make-scalar x-coord) (make-scalar y-coord)))

(defmethod ln ((a-point point) (b-point point))
  (make-line a-point b-point))

(defun lns (point-list)
  (make-line-strip point-list))

(defmethod ln-shape ((origin point) distance-list &key (direction-x-p t))
  (let ((current-x (get-value (get-x origin)))
        (current-y (get-value (get-y origin))))
    (lns (cons origin (mapcar (lambda (distance)
                                (prog1
                                    (if direction-x-p
                                        (make-point (make-scalar (incf current-x distance))
                                            (make-scalar current-y))
                                        (make-point (make-scalar current-x)
                                            (make-scalar (incf current-y distance))))
                                  (setf direction-x-p (not direction-x-p))))
                              distance-list)))))


(defgeneric cp (object anchor target))

(defmethod cp ((obj point) (anchor point) (target point))
    (add obj (subtract target anchor)))

(defmethod cp ((obj line) (anchor point) (target point))
  (make-line (cp (origin obj) anchor target)
      (cp (destination obj) anchor target)))

(defmethod cp ((obj line-strip) (anchor point) (target point))
  (make-line-strip (loop for point in (point-list obj)
                         collect (cp point anchor target))))

(defmethod cp ((obj circle) (anchor point) (target point))
  (make-circle (cp (center obj) anchor target) (radius obj)))

(defmethod cp ((obj group) (anchor point) (target point))
  (make-group (loop for element in (content obj)
                    collect (cp element anchor target))))


;; to be reworked and abstracted
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
          do (setf (aref result i) (cp obj anchor (add target (scale delta i)))))
    result))
