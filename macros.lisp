(in-package :drawer)

(defun get-type-of-elements (element-list)
  (remove-duplicates (loop for element in element-list collect (type-of element))))

(defun draw-with-multiple-backends (backend-list object-list)
  (dolist (object object-list)
    (dolist (backend backend-list)
      (draw object backend)))
  (dolist (backend backend-list 'done)
    (write-file backend)))

(defmacro with-vertical-flip ((height factor) &body body)
  `(let ((*global-point-transformer* (lambda (p)
                                       (make-point (make-scalar (get-value (get-x p)))
                                           (make-scalar (- (/ ,height ,factor) (get-value (get-y p))))))))
     ,@body))
