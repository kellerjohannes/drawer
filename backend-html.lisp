(in-package :drawer)

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
  (setf (scale-factor backend) 10)
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
  (let ((*global-scale-factor* (scale-factor backend)))
    (with-vertical-flip ((height backend) *global-scale-factor*)
      (add-script-line "ctx.beginPath();" backend)
      (add-script-line (format nil "ctx.moveTo(~f, ~f);"
                               (value (x (origin obj)))
                               (value (y (origin obj))))
                       backend)
      (add-script-line (format nil "ctx.lineTo(~f, ~f);"
                               (value (x (destination obj)))
                               (value (y (destination obj))))
                       backend)
      (add-script-line "ctx.stroke();" backend))))

(defmethod draw ((obj line-strip) (backend backend-html))
  (let ((*global-scale-factor* (scale-factor backend)))
    (with-vertical-flip ((height backend) *global-scale-factor*)
      (with-accessors ((points point-list))
          obj
        (add-script-line "ctx.beginPath();" backend)
        (add-script-line (format nil "ctx.moveTo(~f, ~f);"
                                 (value (x (first points)))
                                 (value (y (first points))))
                         backend)
        (dolist (point (rest points))
          (add-script-line (format nil "ctx.lineTo(~f, ~f);"
                                   (value (x point))
                                   (value (y point)))
                           backend))
        (add-script-line "ctx.closePath();" backend)
        (add-script-line "ctx.stroke();" backend)))))

(defmethod draw ((obj circle) (backend backend-html))
  (let ((*global-scale-factor* (scale-factor backend)))
    (with-vertical-flip ((height backend) *global-scale-factor*)
      (add-script-line "ctx.beginPath();" backend)
      (add-script-line (format nil "ctx.arc(~f, ~f, ~f, 0, Math.PI * 2, true);"
                               (value (x (center obj)))
                               (value (y (center obj)))
                               (value (radius obj)))
                       backend)
      (add-script-line "ctx.stroke();" backend))))

(defmethod draw ((obj text) (backend backend-html)))
