(in-package :drawer)

(let ((system (make-tonnetz '(6 6 6) '(0 1 0))))
  (init-row system '(0 nil 0) 0 '("A♭" "C" "E" "G♯" "B♯"))
  (init-row system '(1 nil 0) 0 '("E♭ʼ" "Gʼ" "B♮ʼ" "D♯ʼ"))
  (init-row system '(1 nil 1) 0 '("E♭" "G" "B♮" "D♯"))
  (init-row system '(2 nil 1) -1 '("G♭ʼ" "B♭ʼ" "D'" "F♯ʼ" "A♯ʼ"))
  (init-row system '(2 nil 2) -1 '("G♭" "B♭" "D" "F♯" "A♯"))
  (init-row system '(3 nil 2) -1 '("D♭ʼ" "Fʼ" "Aʼ" "C♯ʼ"))
  (init-row system '(3 nil 3) -1 '("D♭" "F" "A" "C♯" "E♯"))
  (init-row system '(4 nil 3) -1 '("A♭ʼ" "Cʼ" "Eʼ" "G♯ʼ"))
  (init-row system '(4 nil 4) -1 '("(A♭)" "(C)" "(E)" "(G♯)" "(B♯)"))
  (add-connections system '(1 0 0) :style-update '(:line-thickness :thick))
  (add-connections system '(0 1 0) :style-update '(:line-thickness :thick))
  (add-connections system '(1 -1 0) :style-update '(:line-thickness :thick))
  (add-connections system '(0 0 1) :style-update '(:line-type :dotted))
  (let* ((btikz (make-backend-tikz :filename "tonnetz-adaptiv-rein.tex"))
         (btikz-2 (make-backend-tikz :filename "tonnetz-adaptiv-rein-alt.tex"))
         (ts (render system (pt 0 0) (list (pt 15 0)
                                           (pt 0 15)
                                           (rotate-point (pt 0 8.5) -25))
                     2))
         (ts-2 (render system (pt 0 0) (list (pt 15 0)
                                             (rotate-point (pt 0 15) -30)
                                           (rotate-point (pt 0 5.5) -65))
                     2)))
    (draw-with-multiple-backends (list btikz) (list ts))
    (draw-with-multiple-backends (list btikz-2) (list ts-2))
    (compile-tikz btikz)
    (compile-tikz btikz-2)
    ))
