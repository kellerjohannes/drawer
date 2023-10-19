(in-package :drawer)



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
