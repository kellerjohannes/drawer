(in-package :drawer)


;; vicentino adaptiv-rein 1/4-komma
(let* ((center (pt 50 50))
       (origin (pt 0 0))
       (btikz (make-backend-tikz :filename "circle-adaptive-just-quartercomma.tex"))
       (tick (circ 0 0 .4 :style-update '(:fill :fill)))
       (qz-lower (cof center 40 180 0 tick
                      '("G♭" "D♭" "A♭" "E♭" "B♭" "F" "C" "G" "D" "A" "E" "B♮" "F♯" "C♯" "G♯" "D♯" "A♯" "E♯" "B♯")
                      -2.5 6))
       (qz-upper (cof center 30 180 0 tick
                      '("G♭'" "D♭'" "A♭'" "E♭'" "B♭'" "F'" "C'" "G'" "D'" "A'" "E'" "B♮'" "F♯'" "C♯'" "G♯'" "D♯'" "A♯'" nil nil)
                      2.5 6))
       (leg-align (pt -7 8))
       (leg-vspace (pt 0 -3))
       (leg1-lbl (make-text "Reine grosse Terzen" origin :h-align :left))
       (leg1-line (make-line (left-of origin 4) origin))
       (leg2-lbl (make-text "Reine Quinten" origin :h-align :left))
       (leg2-line (ln (left-of origin 4) origin :style-update '(:line-type :dashed)))
       (leg3-lbl (make-text "Reine kleine Terzen" origin :h-align :left))
       (leg3-line (ln (left-of origin 4) origin :style-update '(:line-type :dotted)))
       (leg (gr (list (cp leg1-lbl origin (add center leg-align))
                      (cp leg1-line origin (add center leg-align))
                      (cp leg2-lbl origin (add leg-vspace (add center leg-align)))
                      (cp leg2-line origin (add leg-vspace (add center leg-align)))
                      (cp leg3-lbl origin (add (scale leg-vspace 2) (add center leg-align)))
                      (cp leg3-line origin (add (scale leg-vspace 2) (add center leg-align)))
                      ))))
  (add-circle-connections qz-lower 1 -6 :cof-b qz-upper
                                        :stop 9
                                        :style-update '(:line-type :dashed))
  (add-circle-connections qz-lower 4 -6)
  (add-circle-connections qz-lower -3 -3 :cof-b qz-upper :style-update '(:line-type :strongly-dotted))
  (draw-with-multiple-backends (list btikz) (list qz-lower qz-upper leg))
  (compile-tikz btikz))





;; vicentino adaptiv-rein 1/3-komma
(let* ((center (pt 50 50))
       (origin (pt 0 0))
       (btikz (make-backend-tikz :filename "circle-adaptive-just-thirdcomma.tex"))
       (tick (circ 0 0 .4 :style-update '(:fill :fill)))
       (qz-lower (cof (pt 50 50) 40 220 -140 tick
                          '("G♭" "D♭" "A♭" "E♭" "B♭" "F" "C" "G" "D" "A" "E" "B♮" "F♯" "C♯" "G♯" "D♯" "A♯" "E♯" "B♯" nil)
                          -2.5 6))
       (qz-upper (cof (pt 50 50) 25 270 -90 tick
                          '("G♭'" "D♭'" "A♭'" "E♭'" "B♭'" "F'" "C'" "G'" "D'" "A'" "E'" "B♮'" "F♯'" "C♯'" "G♯'" "D♯'" "A♯'" nil nil nil)
                          2.5 6))
       (leg-align (pt -7 2))
       (leg-vspace (pt 0 -3))
       (leg1-lbl (make-text "Reine grosse Terzen" origin :h-align :left))
       (leg1-line (make-line (left-of origin 4) origin))
       (leg2-lbl (make-text "Reine Quinten" origin :h-align :left))
       (leg2-line (ln (left-of origin 4) origin :style-update '(:line-type :dashed)))
       (leg3-lbl (make-text "Reine kleine Terzen" origin :h-align :left))
       (leg3-line (ln (left-of origin 4) origin :style-update '(:line-type :dotted)))
       (leg (gr (list (cp leg1-lbl origin (add center leg-align))
                      (cp leg1-line origin (add center leg-align))
                      (cp leg2-lbl origin (add leg-vspace (add center leg-align)))
                      (cp leg2-line origin (add leg-vspace (add center leg-align)))
                      (cp leg3-lbl origin (add (scale leg-vspace 2) (add center leg-align)))
                      (cp leg3-line origin (add (scale leg-vspace 2) (add center leg-align)))))))

  (add-circle-connections qz-lower 1 -6 :cof-b qz-upper
                                        :stop 9
                                        :style-update '(:line-type :dashed))
  (add-circle-connection qz-lower 12 -6 :cof-b qz-upper :style-update '(:line-type :dashed))

  (add-circle-connections qz-lower 3 -6 :style-update '(:line-type :dotted))
  (add-circle-connection qz-lower 11 -5 :style-update '(:line-type :dotted))
  (add-circle-connection qz-lower 12 -4 :style-update '(:line-type :dotted))

  (add-circle-connections qz-lower 4 -6 :cof-b qz-upper :stop 6)
  (add-circle-connection qz-lower 9 -6 :cof-b qz-upper)
  (add-circle-connection qz-lower 10 -5 :cof-b qz-upper)
  (add-circle-connection qz-lower 11 -4 :cof-b qz-upper)
  (add-circle-connection qz-lower 12 -3 :cof-b qz-upper)

  (draw-with-multiple-backends (list btikz) (list qz-lower qz-upper leg))
  (compile-tikz btikz))



;; 31-teiliger Zirkel für PhD
(let* ((center (pt 0 -38))
       (origin (pt 0 0))
       (tick (circ 0 0 .4 :style-update '(:fill :fill)))
       (qz (cof (pt 0 0) 30 325 -15 tick
                '("Ḃ♮" "G♭" "D♭" "A♭" "E♭" "B♭" "F" "C" "G" "D" "A" "E" "B♮" "F♯" "C♯" "G♯" "D♯" "A♯"
                  "E♯" "B♯" "Ġ♭" "Ḋ♭" "Ȧ♭" "Ė♭" "Ḃ♭" "Ḟ" "Ċ" "Ġ" "Ḋ" "Ȧ" "Ė")
                -2.5 7))
       (leg-align (pt -11 2))
       (leg-vspace (pt 0 -3))
       (leg1-lbl (make-text "diesis enarmonico minore I" origin :h-align :left))
       (leg1-line (make-line (left-of origin 4) origin))
       (leg2-lbl (make-text "diesis enarmonico minore II" origin :h-align :left))
       (leg2-line (ln (left-of origin 4) origin :style-update '(:line-type :dashed)))
       (leg (gr (list (cp leg1-lbl origin (add center leg-align))
                      (cp leg1-line origin (add center leg-align))
                      (cp leg2-lbl origin (add leg-vspace (add center leg-align)))
                      (cp leg2-line origin (add leg-vspace (add center leg-align)))
                      )))
       (btikz (make-backend-tikz :filename "circle-31--1-4-comma.tex")))
  (draw-with-multiple-backends (list btikz) (list qz))
  (compile-tikz btikz)

  (setf btikz (make-backend-tikz :filename "circle-31--1-4-comma-diesis.tex"))

  (add-circle-connections qz 12 -7)
  (add-circle-connections qz 19 -7 :style-update '(:line-type :dashed))

  (draw-with-multiple-backends (list btikz) (list qz leg))
  (compile-tikz btikz)

  (setf btikz (make-backend-tikz :filename "circle-31--1-4-comma-terza-maggiore.tex"))

  (setf leg-align (pt -8 2))
  (setf leg1-lbl (make-text "terza maggiore I" origin :h-align :left))
  (setf leg2-lbl (make-text "terza maggiore II" origin :h-align :left))

  (setf leg (gr (list (cp leg1-lbl origin (add center leg-align))
                      (cp leg1-line origin (add center leg-align))
                      (cp leg2-lbl origin (add leg-vspace (add center leg-align)))
                      (cp leg2-line origin (add leg-vspace (add center leg-align))))))

  (setf (connection-list qz) nil)
  (add-circle-connections qz 4 -7)
  (add-circle-connections qz 27 -7 :style-update '(:line-type :dashed))

  (draw-with-multiple-backends (list btikz) (list qz leg))
  (compile-tikz btikz)
  )
