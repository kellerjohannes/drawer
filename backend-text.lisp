(in-package :drawer)

(defclass backend-text (drawer-backend)
  ())

(defmethod write-file ((backend backend-text))
  ())

(defmethod draw ((obj line) (backend backend-text))
  (format t "~&Drawing a line from (~a,~a) to (~a,~a)"
          (value (x (origin obj)))
          (value (y (origin obj)))
          (value (x (destination obj)))
          (value (y (destination obj)))))

(defmethod draw ((obj line-strip) (backend backend-text)))

(defmethod draw ((obj group) (backend backend-text))
  (format t "~&Drawing a group of ~a objects:"
          (length (content obj)))
  (dolist (element (content obj))
    (draw element backend)))
