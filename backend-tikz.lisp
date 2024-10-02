(in-package :drawer)

(defparameter *tikz-header*
  "\\documentclass[tikz,border=10pt]{standalone}
\\usepackage{mathabx}
\\usepackage{stackengine}
\\usetikzlibrary{backgrounds}
\\usepackage{newunicodechar}
\\newunicodechar{♮}{$\\natural$}
\\newunicodechar{♭}{$\\flat$}
\\newunicodechar{♯}{$\\sharp$}
\\newunicodechar{➚}{$\\nearrow$}
\\newunicodechar{➘}{$\\searrow$}
\\newunicodechar{ʼ}{'}
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


;; \\begin{tikzpicture}[background rectangle/.style={fill=black}, show background rectangle, draw=white, text=white]

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
    (uiop:run-program (list
                       ;; "/usr/bin/pdflatex"
                       "/usr/bin/xelatex"
                       ;; "/usr/local/texlive/2020/bin/x86_64-linux/pdflatex"
                       ;; "/usr/local/texlive/2020/bin/x86_64-linux/xelatex"
                       (namestring
                        (merge-pathnames
                         (filename backend)
                         (merge-pathnames "export/"
                                          (asdf/system:system-source-directory :drawer))))))
    (format t "~&TIKZ compiled.")))

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

(defparameter *tikz-h-align*
  '((:center . "align=center")
    (:left . "anchor=west")
    (:right . "anchor=east")))

(defun lookup-h-align (keyword)
  (cdr (assoc keyword *tikz-h-align*)))


(defmethod draw ((obj arc-label) (backend backend-tikz))
  (let ((*global-scale-factor* (scale-factor backend)))
    (with-accessors ((a point-a)
                     (b point-b))
        obj
      (let* ((dist (distance-between-points a b))
             (offset-1 (* (steepness obj) dist))
             (radius-1 (hyp (* 1/2 dist) offset-1))
             (end-angle-1 (rad-to-deg (asin (/ offset-1 radius-1))))
             (start-angle-1 (+ end-angle-1 (* 2 (rad-to-deg (asin (/ (* 1/2 dist) radius-1))))))

             (offset-2 (+ offset-1 (height obj)))
             (radius-2 (hyp (* 1/2 dist) offset-2))
             (end-angle-2 (rad-to-deg (asin (/ offset-2 radius-2))))
             (start-angle-2 (+ end-angle-2 (* 2 (rad-to-deg (asin (/ (* 1/2 dist) radius-2))))))
             (label-y (* (if (inversep obj) -1 1)
                         (+ (if (inversep obj) (value (make-scalar (inverse-thickness obj))) 0)
                            (* 1/2 (+ (value (y a)) (- radius-1 offset-1)
                                      (value (y a)) (- radius-2 offset-2)))))))
        (add-tikz-line (format nil "\\draw[fill=white] (~f,~f) arc[start angle=~f, end angle=~f, radius=~f] arc[start angle=~f, end angle=~f, radius=~f] -- cycle;"
                               (value (x a))
                               (if (inversep obj)
                                   (value (y (below a (inverse-thickness obj))))
                                   (value (y a)))
                               (if (inversep obj) (- start-angle-1) start-angle-1)
                               (if (inversep obj) (- end-angle-1) end-angle-1)
                               radius-1
                               (if (inversep obj) (- end-angle-2) end-angle-2)
                               (if (inversep obj) (- start-angle-2) start-angle-2)
                               radius-2)
                       backend)
        (add-tikz-line (format nil "\\node[~a] at (~f,~f) { \\tiny ~a };"
                               (lookup-h-align :center)
                               (+ (value (x a)) (* 1/2 dist))
                               label-y
                               (label obj))
                       backend)))))



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
