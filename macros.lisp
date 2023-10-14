(in-package :drawer)

(defun draw-with-multiple-backends (backend-list object-list)
  (dolist (object object-list)
    (dolist (backend backend-list)
      (draw object backend)))
  (dolist (backend backend-list)
    (write-file backend)))
