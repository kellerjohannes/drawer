(in-package :drawer)



(defun pt (x-coord y-coord)
  (make-point (make-scalar x-coord) (make-scalar y-coord)))

(defmethod ln ((a-point point) (b-point point))
  (make-line a-point b-point))

(defun lns (point-list)
  (make-line-strip point-list))

(defun make-shifted-point (x y shift-amount shift-pt)
  (if (and shift-amount shift-pt)
      (let ((shift-x (get-value (get-x shift-pt)))
            (shift-y (get-value (get-y shift-pt))))
        (make-point (make-scalar (funcall (if (> x shift-x) #'- #'+) x shift-amount))
            (make-scalar (funcall (if (> y shift-y) #'- #'+) y shift-amount))))
      (make-point (make-scalar x) (make-scalar y))))

(defun shift-point (pt shift-amount shift-pt)
  (make-shifted-point (get-value (get-x pt))
                      (get-value (get-y pt))
                      shift-amount
                      shift-pt))

(defmethod ln-shape ((origin point) distance-list &key (direction-x-p t) shift shift-pt)
  (let ((current-x (get-value (get-x origin)))
        (current-y (get-value (get-y origin))))
    (lns (cons (shift-point origin shift shift-pt)
               (mapcar (lambda (distance)
                         (prog1
                             (if direction-x-p
                                 (make-shifted-point (incf current-x distance)
                                                     current-y shift shift-pt)
                                 (make-shifted-point current-x (incf current-y distance)
                                                     shift shift-pt))
                           (setf direction-x-p (not direction-x-p))))
                       distance-list)))))


(defun gr (content-list)
  (make-group content-list))




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

(defmethod cp ((obj text) (anchor point) (target point))
  (make-text (text-string obj) (cp (anchor obj) anchor target)
             :v-align (vertical-alignment obj)
             :h-align (horizontal-alignment obj)))

(defmethod cp ((obj group) (anchor point) (target point))
  (make-group (loop for element in (content obj)
                    collect (cp element anchor target))))

(defmethod cp ((obj vector) (anchor point) (target point))
  (let ((result (make-array (length obj))))
    (loop for i from 0 to (1- (length obj))
          do (setf (aref result i) (cp (aref obj i) anchor target)))
    result))

;; to be reworked and abstracted
(defmethod above ((origin point) distance)
  (make-point (get-x origin) (make-scalar (+ (get-value (get-y origin)) distance))))

(defmethod above ((origin point) (distance scalar))
  (above origin (get-value distance)))


(defmethod below ((origin point) (distance scalar))
  (make-point (x origin) (make-scalar (- (value (y origin)) (value distance)))))


(defmethod right-of ((origin point) distance)
  (make-point (make-scalar (+ (get-value (get-x origin)) distance)) (get-y origin)))

(defmethod right-of ((origin point) (distance scalar))
  (right-of origin (get-value distance)))


(defmethod left-of ((origin point) (distance scalar))
  (make-point (make-scalar (- (value (x origin)) (value distance))) (y origin)))


(defmethod make-obj-array ((obj drawer-object) iterations (anchor point) (target point) (delta point))
  (let ((result (make-array iterations :element-type (type-of obj))))
    (loop for i from 0 to (1- iterations)
          do (setf (aref result i) (cp obj anchor (add target (scale delta i)))))
    result))


(defmethod make-text-array (text-string-list (first-position point) (delta point)
                            &key (h-align :center) (v-align :center))
  (let ((result (make-array (length text-string-list))))
    (loop for text-string in text-string-list
          for i from 0
          do (setf (aref result i)
                   (make-text text-string
                              (make-point (make-scalar (+ (get-value (get-x first-position))
                                                          (* i (get-value (get-x delta)))))
                                  (make-scalar (+ (get-value (get-y first-position))
                                                  (* i (get-value (get-y delta))))))
                              :v-align v-align :h-align h-align)))
    result))
