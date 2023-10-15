(in-package :drawer)

(defclass backend-svg (drawer-backend)
  ((scene :accessor scene)
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height)))

(defmethod initialize-instance :after ((backend backend-svg) &key)
  (setf (scale-factor backend) 10)
  (setf (scene backend) (svg:make-svg-toplevel 'svg:svg-1.2-toplevel
                                               :width (width backend)
                                               :height (height backend)
                                               :stroke "black" :fill "none")))

(defmethod make-backend-svg (&optional (width 1200) (height 800))
  (make-instance 'backend-svg :width width :height height :filename "default.svg"))

(defmethod write-file ((backend backend-svg))
  (svg:stream-out (filestream backend) (scene backend)))

(defmethod draw ((obj line) (backend backend-svg))
  (let ((*global-scale-factor* (scale-factor backend)))
    (with-vertical-flip ((height backend) *global-scale-factor*)
      (svg:draw (scene backend) (:line :x1 (value (x (origin obj)))
                                       :y1 (value (y (origin obj)))
                                       :x2 (value (x (destination obj)))
                                       :y2 (value (y (destination obj))))))))

(defmethod draw ((obj line-strip) (backend backend-svg))
  (let ((*global-scale-factor* (scale-factor backend)))
    (with-vertical-flip ((height backend) *global-scale-factor*)
      (with-accessors ((points point-list))
          obj
        (svg:add-element (scene backend)
                         (format nil "<path d=\"M ~f ~f ~{L ~f ~f ~}Z\"/>"
                                 (value (x (first points)))
                                 (value (y (first points)))
                                 (extract-value-list (rest points))))))))

(defmethod draw ((obj circle) (backend backend-svg))
  (let ((*global-scale-factor* (scale-factor backend)))
    (with-vertical-flip ((height backend) *global-scale-factor*)
      (svg:draw (scene backend) (:circle :cx (value (x (center obj)))
                                         :cy (value (y (center obj)))
                                         :r (value (radius obj)))))))

(defmethod draw ((obj text) (backend backend-svg))
  (when (text-string obj)
    (let ((*global-scale-factor* (scale-factor backend)))
      (with-vertical-flip ((height backend) *global-scale-factor*)
        (svg:text (scene backend) (:x (value (x (anchor obj)))
                                   :y (value (y (anchor obj)))
                                   :font-size 20
                                   :fill "black"
                                   :text-anchor "middle"
                                   :dominant-baseline "middle")
          (text-string obj))))))
