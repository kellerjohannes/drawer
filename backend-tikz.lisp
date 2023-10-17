(in-package :drawer)

(defparameter *tikz-header*
  "\\documentclass[tikz,border=10pt]{standalone}
\\usepackage{newunicodechar}
\\newunicodechar{♮}{$\\natural$}
\\newunicodechar{♭}{$\\flat$}
\\newunicodechar{♯}{$\\sharp$}
\\newunicodechar{➚}{$\\nearrow$}
\\newunicodechar{➘}{$\\searrow$}
\\newunicodechar{Ȧ}{\\stackon[0.8pt]{A}{.}}
\\newunicodechar{Ḃ}{\\stackon[0.8pt]{B}{.}}
\\newunicodechar{Ċ}{\\stackon[0.8pt]{C}{.}}
\\newunicodechar{Ḋ}{\\stackon[0.8pt]{D}{.}}
\\newunicodechar{Ė}{\\stackon[0.8pt]{E}{.}}
\\newunicodechar{Ḟ}{\\stackon[0.8pt]{F}{.}}
\\newunicodechar{Ġ}{\\stackon[0.8pt]{G}{.}}
\\begin{document}
\\begin{tikzpicture}
")

(defclass backend-tikz (drawer-backend)
  ((scene :accessor scene)
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (compilep :initform t :initarg :compilep :accessor compilep)))

(defmethod add-tikz-line (expression (backend backend-tikz))
  (setf (scene backend) (concatenate 'string (scene backend) (format nil "~%") expression)))

(defmethod initialize-instance :after ((backend backend-tikz) &key)
  (setf (scale-factor backend) 0.2)
  (setf (scene backend) *tikz-header*))

(defmethod make-backend-tikz (&optional (width 1200) (height 800))
  (make-instance 'backend-tikz :width width :height height :filename "default.tex"))

(defmethod write-file ((backend backend-tikz))
  (format (filestream backend) "~a~%\\end{tikzpicture}~%\\end{document}" (scene backend)))

(defmethod compile-tikz ((backend backend-tikz))
  (when (compilep backend)
    (uiop:run-program (list "/usr/bin/pdflatex"
                            "/home/johannes/common-lisp/prototypes/drawer/default.tex"))))

(defmethod draw ((obj line) (backend backend-tikz))
  (let ((*global-scale-factor* (scale-factor backend)))
    (add-tikz-line (format nil "\\draw[thick] (~f,~f) -- (~f,~f);"
                           (value (x (origin obj)))
                           (value (y (origin obj)))
                           (value (x (destination obj)))
                           (value (y (destination obj))))
                   backend)))

(defmethod draw ((obj line-strip) (backend backend-tikz))
  (let ((*global-scale-factor* (scale-factor backend)))
    (with-accessors ((points point-list))
        obj
      (add-tikz-line (format nil "\\draw[thick] ~{(~f, ~f) -- ~} cycle;"
                             (extract-value-list points))
                     backend))))

(defmethod draw ((obj circle) (backend backend-tikz))
  (let ((*global-scale-factor* (scale-factor backend)))
    (add-tikz-line (format nil "\\draw[thick] (~f,~f) circle (~f);"
                           (value (x (center obj)))
                           (value (y (center obj)))
                           (value (radius obj)))
                   backend)))

(defmethod draw ((obj text) (backend backend-tikz))
  (when (text-string obj)
    (let ((*global-scale-factor* (scale-factor backend)))
      (add-tikz-line (format nil "\\node at (~f, ~f) { \\large ~a };"
                             (value (x (anchor obj)))
                             (value (y (anchor obj)))
                             (text-string obj))
                     backend))))
