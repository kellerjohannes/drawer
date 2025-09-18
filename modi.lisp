(in-package :drawer)

(defun step-group (origin width vertical-step steps label)
  (let ((x-orig (value (x origin)))
        (y-orig (value (y origin))))
    (gr (list (lns (list origin
                         (pt (+ x-orig width) y-orig)
                         (pt (+ x-orig width) (+ y-orig (* steps vertical-step)))
                         (pt x-orig (+ y-orig (* steps vertical-step)))))
              (make-text label
                         (pt (+ x-orig (* 1/2 width))
                             (+ y-orig (* 1/2 steps vertical-step)))
                         :angle 90)))))

(defun grid (lowest leftest rightest vertical-steps labels)
  (let ((y lowest))
    (gr (mapcar (lambda (vertical-step label)
                  (prog1 (gr (list (make-text label (pt leftest y) :h-align :right)
                                   (make-text label (pt rightest y) :h-align :left)
                                   (ln (pt leftest y)
                                       (pt rightest y)
                                       :style-update '(:line-type :dotted))))
                    (incf y vertical-step)))
                vertical-steps
                labels))))

(defun quinta (y-origin width vertical-step label)
  (step-group (pt 0 y-origin) width vertical-step 18 label))

(defun quarta (y-origin width vertical-step label)
  (step-group (pt 0 y-origin) width vertical-step 13 label))

(defun ottava (first-group second-group horizontal-offset label lbl-width lbl-y)
  (cp (gr (list first-group
                second-group
                (make-text label (pt (* 1/2 lbl-width) lbl-y))))
      (pt 0 0) (pt horizontal-offset 0)))

(defun modo (termini x-offset radius label label-y-offset)
  (let ((last-y (first termini)))
    (gr (list (gr (mapcar (lambda (y)
                            (circ x-offset y radius :style-update '(:fill :fill)))
                          termini))
              (gr (mapcar (lambda (y)
                            (prog1 (ln (pt x-offset last-y)
                                       (pt x-offset y) :style-update '(:line-thickness :thick))
                              (setf last-y y)))
                          (rest termini)))
              (make-text label (pt x-offset (+ last-y label-y-offset)) :angle 90)))))

(let* ((vertical-step 1)
       (bar-width 5)
       (horizontal-grid-space 7.5)
       (grid-lines (grid -18
                         -6
                         100
                         (list 5 5 3 5 5 3 5 5 5 3 5 5 3 5 5 5 3 5 5 3)
                         (list "G" "A" "B♮" "C" "D" "E" "F" "G" "A" "B♮" "C" "D" "E" "F" "G" "A")))
       (quarta-prima (quarta -13 bar-width vertical-step "prima"))
       (quarta-seconda (quarta -8 bar-width vertical-step "seconda"))
       (quarta-terza (quarta -5 bar-width vertical-step "terza"))
       (quinta-prima (quinta 0 bar-width vertical-step "prima"))
       (quinta-seconda (quinta 5 bar-width vertical-step "seconda"))
       (quinta-terza (quinta 8 bar-width vertical-step "terza"))
       (quinta-quarta (quinta 13 bar-width vertical-step "quarta"))

       (ottava-prima (ottava quarta-prima quinta-prima
                             (* 0 horizontal-grid-space)
                             "I" bar-width -15.5))
       (ottava-seconda (ottava quarta-seconda quinta-seconda
                               (* 1 horizontal-grid-space)
                               "II" bar-width -15.5))
       (ottava-terza (ottava quarta-terza quinta-terza
                             (* 2 horizontal-grid-space)
                             "III" bar-width -15.5))
       (ottava-quarta (ottava quinta-prima (cp quarta-prima (pt 0 0) (pt 0 31))
                              (* 3 horizontal-grid-space)
                              "IV" bar-width -15.5))
       (ottava-quinta (ottava quinta-seconda (cp quarta-seconda (pt 0 0) (pt 0 31))
                              (* 4 horizontal-grid-space)
                              "V" bar-width -15.5))
       (ottava-sesta (ottava quinta-terza (cp quarta-terza (pt 0 0) (pt 0 31))
                             (* 5 horizontal-grid-space)
                             "VI" bar-width -15.5))
       (ottava-settima (ottava quinta-quarta (cp quarta-prima (pt 0 0) (pt 0 (+ 31 13)))
                               (* 6 horizontal-grid-space)
                               "VII" bar-width -15.5))
       (point-size 1)
       (primo-modo (modo (list 0 18 31)
                         (* 8 horizontal-grid-space)
                         point-size
                         "primo modo" 7.5))
       (secondo-modo (modo (list -13 0 18)
                         (* 8.5 horizontal-grid-space)
                         point-size
                         "secondo modo" 8.5))
       (terzo-modo (modo (list 5 23 36)
                         (* 9.5 horizontal-grid-space)
                         point-size
                         "terzo modo" 7))
       (quarto-modo (modo (list -8 5 23)
                         (* 10 horizontal-grid-space)
                         point-size
                         "quarto modo" 8))
       (quinto-modo (modo (list 8 26 39)
                         (* 11 horizontal-grid-space)
                         point-size
                         "quinto modo" 7.5))
       (sesto-modo (modo (list -5 8 26)
                         (* 11.5 horizontal-grid-space)
                         point-size
                         "sesto modo" 7))
       (settimo-modo (modo (list 13 31 44)
                         (* 12.5 horizontal-grid-space)
                         point-size
                         "settimo modo" 8.5))
       (ottavo-modo (modo (list 0 13 31)
                         (* 13 horizontal-grid-space)
                         point-size
                         "ottavo modo" 7.5))

       (btikz (make-backend-tikz :filename "vicentino-modi-semplici.tex")))
  (draw-with-multiple-backends (list btikz) (list grid-lines
                                                  ottava-prima
                                                  ottava-seconda
                                                  ottava-terza
                                                  ottava-quarta
                                                  ottava-quinta
                                                  ottava-sesta
                                                  ottava-settima
                                                  primo-modo
                                                  secondo-modo
                                                  terzo-modo
                                                  quarto-modo
                                                  quinto-modo
                                                  sesto-modo
                                                  settimo-modo
                                                  ottavo-modo
                                                  ))
  (compile-tikz btikz))
