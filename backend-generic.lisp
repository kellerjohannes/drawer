(in-package :drawer)

(defclass drawer-backend ()
  ((filename :initarg :filename :accessor filename)
   (path :initarg :path :accessor path)
   (filestream :accessor filestream)
   (scale-factor :initform 1 :accessor scale-factor)))

(defgeneric write-file (drawer-backend))

(defmethod write-file :around ((backend drawer-backend))
  (setf (filestream backend) (open (merge-pathnames (filename backend)
                                                    (path backend))
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create))
  (call-next-method)
  (close (filestream backend)))

(defgeneric draw (visible-object backend))

(defmethod draw ((obj group) (backend drawer-backend))
  (dolist (element (content obj))
    (draw element backend)))

(defmethod draw (arr (backend drawer-backend))
  (loop for element across arr
        do (draw element backend)))
