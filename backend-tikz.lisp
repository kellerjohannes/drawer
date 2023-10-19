(in-package :drawer)

(defparameter *tikz-header*
  "\\documentclass[tikz,border=10pt]{standalone}
\\usepackage{mathabx}
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


\\def\\centerarc[#1](#2)(#3:#4:#5);%
{
  \\draw[#1]([shift=(#3:#5)]#2) arc (#3:#4:#5);
}


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

(defun make-backend-tikz (&key (width 1200) (height 800) (filename "default.tex"))
  (make-instance 'backend-tikz :width width :height height :filename filename))

(defmethod write-file ((backend backend-tikz))
  (format (filestream backend) "~a~%\\end{tikzpicture}~%\\end{document}" (scene backend)))

(defmethod compile-tikz ((backend backend-tikz))
  (when (compilep backend)
    (uiop:run-program (list ;"/usr/bin/pdflatex"
                            "/usr/local/texlive/2020/bin/x86_64-linux/pdflatex"
                            (concatenate 'string "/home/johannes/common-lisp/prototypes/drawer/"
                                           (filename backend))))))

(defparameter *tikz-dictionary*
  '((:normal . nil)
    (:thick . "thick")
    (:thin . "very thin")
    (:dotted . "dotted")
    (:dashed . "dashed")
    (:strongly-dotted . "dotted")))

(defun lookup-style (keyword)
  (cdr (assoc keyword *tikz-dictionary*)))

(defun combine-line-styles (style)
  (let ((result (apply #'concatenate 'string
         (remove-if #'null (mapcar (lambda (id)
                                     (lookup-style (getf style id)))
                                   '(:line-thickness :line-type))))))
    (if (zerop (length result))
        nil
        result)))

(defmethod draw ((obj line) (backend backend-tikz))
  (let ((*global-scale-factor* (scale-factor backend)))
    (add-tikz-line (format nil "\\draw~@[[~a]~] (~f,~f) -- (~f,~f);"
                           (combine-line-styles (style obj))
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
    (add-tikz-line (format nil "\\draw[~a] (~f,~f) circle (~f);"
                           (if (eq (getf (style obj) :fill) :none) "" "fill")
                           (value (x (center obj)))
                           (value (y (center obj)))
                           (value (radius obj)))
                   backend)))

(defmethod draw ((obj arc) (backend backend-tikz))
  (let ((*global-scale-factor* (scale-factor backend)))
    (case (mode obj)
      (:center (add-tikz-line (format nil "\\centerarc[thick](~a,~a)(~a:~a:~a);"
                                      (value (x (center obj)))
                                      (value (y (center obj)))
                                      (start-angle obj)
                                      (end-angle obj)
                                      (value (radius obj)))
                              backend))
      (:point (add-tikz-line (format nil "\\draw (~a,~a) arc (~a:~a:~a);"
                                     (value (x (center obj)))
                                     (value (y (center obj)))
                                     (start-angle obj)
                                     (end-angle obj)
                                     (value (radius obj)))
                             backend)))))

(defmethod draw ((obj arc-label) (backend backend-tikz))
  (let ((*global-scale-factor* (scale-factor backend)))
    (with-accessors ((a point-a)
                     (b point-b))
        obj
        (add-tikz-line (format nil "\\draw(~a,~a) arc[start angle=160, end angle=20, radius=~a];"
                               (value (x a))
                               (value (y a))
                               (* 1/2 (distance-between-points a b)))
                       backend))))


(defparameter *tikz-h-align*
  '((:center . "align=center")
    (:left . "anchor=west")
    (:right . "anchor=east")))

(defun lookup-h-align (keyword)
  (cdr (assoc keyword *tikz-h-align*)))

(defmethod draw ((obj text) (backend backend-tikz))
  (when (text-string obj)
    (let ((*global-scale-factor* (scale-factor backend)))
      (add-tikz-line (format nil "\\node[~a] at (~f, ~f) { \\large ~a };"
                             (lookup-h-align (horizontal-alignment obj))
                             (value (x (anchor obj)))
                             (value (y (anchor obj)))
                             (text-string obj))
                     backend))))


(defmethod draw ((obj circle-of-fifths) (backend backend-tikz))
  (when (connection-list obj)
    (dolist (connection (connection-list obj))
      (draw connection backend)))
  (when (tick-array obj)
    (loop for tick across (tick-array obj)
          do (draw tick backend)))
  (when (label-array obj)
    (loop for label across (label-array obj)
          do (draw label backend)))
  (when (circle-line obj)
    (draw (circle-line obj) backend)))
