(in-package :drawer)

(defun get-type-of-elements (element-list)
  (remove-duplicates (loop for element in element-list collect (type-of element))))

(defun draw-with-multiple-backends (backend-list object-list)
  (dolist (object object-list)
    (dolist (backend backend-list)
      (draw object backend)))
  (dolist (backend backend-list 'done)
    (write-file backend)))
