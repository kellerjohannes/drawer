(in-package :drawer)



(let* ((btikz (make-backend-tikz :filename "circle-adaptive-just-quartercomma.tex"))
       (tick (circ 0 0 .4 :style-update '(:fill :fill)))
       (qz-lower (cof (pt 50 50) 40 180 0 tick
                      '("G♭" "D♭" "A♭" "E♭" "B♭" "F" "C" "G" "D" "A" "E" "B♮" "F♯" "C♯" "G♯" "D♯" "A♯" "E♯" "B♯") -2.5 7))
       (qz-upper (cof (pt 50 50) 30 180 0 tick
                      '("G♭" "D♭" "A♭" "E♭" "B♭" "F" "C" "G" "D" "A" "E" "B♮" "F♯" "C♯" "G♯" "D♯" "A♯") 2.5 7)))
  (draw-with-multiple-backends (list btikz) (list qz-lower qz-upper))
  (compile-tikz btikz))



(let* ((btikz (make-backend-tikz :filename "circle-adaptive-just-thirdcomma.tex"))
       (tick (circ 0 0 .4 :style-update '(:fill :fill)))
       (qz-lower (cof (pt 50 50) 40 220 -140 tick
                      '("G♭" "D♭" "A♭" "E♭" "B♭" "F" "C" "G" "D" "A" "E" "B♮" "F♯" "C♯" "G♯" "D♯" "A♯" "E♯" "B♯" nil) -2.5 7))
       (qz-upper (cof (pt 50 50) 30 220 -140 tick
                      '("G♭" "D♭" "A♭" "E♭" "B♭" "F" "C" "G" "D" "A" "E" "B♮" "F♯" "C♯" "G♯" "D♯" "A♯" nil nil nil) 2.5 7)))
  (draw-with-multiple-backends (list btikz) (list qz-lower qz-upper))
  (compile-tikz btikz))
