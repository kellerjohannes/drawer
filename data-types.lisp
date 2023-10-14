(in-package :drawer)


(defclass drawer-object ()
  ())


(defclass scalar (drawer-object)
  ((value :initarg :value :accessor value)))

(defmethod print-object ((object scalar) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "value: ~a" (value object))))

(defmethod make-scalar (value)
  (make-instance 'scalar :value value))

(defmethod add ((a scalar) (b scalar))
  (make-scalar (+ (value a) (value b))))

(defmethod subtract ((a scalar) (b scalar))
  (make-scalar (- (value b) (value a))))

(defmethod scale ((s scalar) factor)
  (make-scalar (* factor (value s))))



(defclass point (drawer-object)
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)))

(defmethod print-object ((object point) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "x: ~a, y: ~a" (x object) (y object))))

(defmethod make-point (x y)
  (make-instance 'point :x x :y y))

(defmethod add ((a point) (b point))
  (make-point (add (x a) (x b))
      (add (y a) (y b))))

(defmethod subtract ((a point) (b point))
  (make-point (subtract (x b) (x a))
      (subtract (y b) (y a))))

(defmethod scale ((p point) factor)
  (make-point (scale (x p) factor)
      (scale (y p) factor)))


(defclass visible-object (drawer-object)
  ())


(defclass line (visible-object)
  ((origin :initarg :origin :accessor origin)
   (destination :initarg :destination :accessor destination)))

(defmethod print-object ((object line) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "origin: ~a, destination: ~a" (origin object) (destination object))))

(defmethod make-line (origin destination)
  (make-instance 'line :origin origin :destination destination))




(defun extract-value-list (point-list)
  (loop for point in point-list
        append (list (value (x point))
                     (value (y point)))))

(defun get-type-of-elements (element-list)
  (remove-duplicates (loop for element in element-list collect (type-of element))))

(defclass line-strip (visible-object)
  ((point-list :initarg :point-list :accessor point-list)))

(defmethod print-object ((object line-strip) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "Line-strip: [~{(~a, ~a) ~}]"
            (extract-value-list (point-list object)))))

(defmethod make-line-strip (point-list)
  (format t "~&~a" (get-type-of-elements point-list))
  (if (equal (get-type-of-elements point-list) (list 'point))
      (make-instance 'line-strip :point-list point-list)
      (error "Point-list ~a does not consist of exclusively points." point-list)))




(defclass circle (visible-object)
  ((radius :initarg :radius :accessor radius)
   (center :initarg :center :accessor center)))

(defmethod print-object ((object circle) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "center: (~a,~a), radius: ~a"
            (value (x (center object)))
            (value (y (center object)))
            (value (radius object)))))

(defmethod make-circle ((center point) (radius scalar))
  (make-instance 'circle :center center :radius radius))



(defclass structuring-object (visible-object)
  ())

(defclass group (structuring-object)
  ((content :initarg :content :accessor content)))

(defmethod print-object ((object group) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "content: ~a" (content object))))

(defmethod make-group (content)
  (make-instance 'group :content content))




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





(defclass drawer-backend ()
  ((filename :initarg :filename :accessor filename)
   (filestream :accessor filestream)))

(defgeneric write-file (drawer-backend))

(defmethod write-file :around ((backend drawer-backend))
  (setf (filestream backend) (open (filename backend)
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





(defclass backend-svg (drawer-backend)
  ((scene :accessor scene)
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height)))

(defmethod initialize-instance :after ((backend backend-svg) &key)
  (setf (scene backend) (svg:make-svg-toplevel 'svg:svg-1.2-toplevel
                                               :width (width backend)
                                               :height (height backend)
                                               :stroke "black" :fill "none")))

(defmethod make-backend-svg (&optional (width 1200) (height 800))
  (make-instance 'backend-svg :width width :height height :filename "default.svg"))

(defmethod write-file ((backend backend-svg))
  (svg:stream-out (filestream backend) (scene backend)))

(defmethod draw ((obj line) (backend backend-svg))
  (svg:draw (scene backend) (:line :x1 (value (x (origin obj)))
                                   :y1 (value (y (origin obj)))
                                   :x2 (value (x (destination obj)))
                                   :y2 (value (y (destination obj))))))

(defmethod draw ((obj circle) (backend backend-svg))
  (svg:draw (scene backend) (:circle :cx (value (x (center obj)))
                                     :cy (value (y (center obj)))
                                     :r (value (radius obj)))))


(defparameter *html-header*
  "<!doctype html>
<html lang=\"en-US\">
  <head>
    <meta charset=\"UTF-8\" />
    <title>Canvas experiment</title>
  </head>
  <body>
")

(defclass backend-html (drawer-backend)
  ((scene :accessor scene)
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height)))

(defmethod add-script-line (expression (backend backend-html))
  (setf (scene backend) (concatenate 'string (scene backend) (format nil "~%") expression)))

(defmethod initialize-instance :after ((backend backend-html) &key)
  (setf (scene backend)
        (concatenate 'string *html-header*
                     (format nil "<canvas id=\"defaultId\" width=\"~a\" height=\"~a\"></canvas>~%<script>"
                             (width backend) (height backend))))
  (add-script-line "function draw() {" backend)
  (add-script-line "const canvas = document.getElementById(\"defaultId\");" backend)
  (add-script-line "const ctx = canvas.getContext(\"2d\");" backend))

(defmethod make-backend-html (&optional (width 1200) (height 800))
  (make-instance 'backend-html :width width :height height :filename "default.html"))

(defmethod write-file ((backend backend-html))
  (format (filestream backend) "~a~%}~%draw();~%</script>~%</body>~%</html>" (scene backend)))

(defmethod draw ((obj line) (backend backend-html))
  (add-script-line "ctx.beginPath();" backend)
  (add-script-line (format nil "ctx.moveTo(~a, ~a);"
                           (value (x (origin obj)))
                           (value (y (origin obj))))
                   backend)
  (add-script-line (format nil "ctx.lineTo(~a, ~a);"
                           (value (x (destination obj)))
                           (value (y (destination obj))))
                   backend)
  (add-script-line "ctx.stroke();" backend)
  )

(defmethod draw ((obj circle) (backend backend-html))
  (add-script-line "ctx.beginPath();" backend)
  (add-script-line (format nil "ctx.arc(~a, ~a, ~a, 0, Math.PI * 2, true);"
                           (value (x (center obj)))
                           (value (y (center obj)))
                           (value (radius obj)))
                   backend)
  (add-script-line "ctx.stroke();" backend))



(defun draw-with-multiple-backends (backend-list object-list)
  (dolist (object object-list)
    (dolist (backend backend-list)
      (draw object backend)))
  (dolist (backend backend-list)
    (write-file backend)))

(let* ((bsvg (make-backend-svg))
       (bhtml (make-backend-html))
       (ax (make-scalar 100))
       (ay (make-scalar 50))
       (bx (make-scalar 800))
       (by (make-scalar 650))
       (p1 (make-point ax ay))
       (p2 (make-point bx by))
       (c0 (make-scalar 0))
       (cy (make-scalar 50))
       (dd (make-scalar 20))
       (o (make-point c0 c0))
       (dot (make-circle p1 cy))
       (delta (make-point c0 cy))
       (mini-delta (make-point dd c0))
       (diagonal (make-line p1 p2))
       (dots (make-obj-array dot 12 o p2 mini-delta))
       (double-line (make-group (list dot
                                      diagonal
                                      (copy-move diagonal o delta)
                                      (copy-move dot o delta)))))
  (draw-with-multiple-backends (list bsvg bhtml) (list double-line dots)))
