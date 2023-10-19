(in-package :drawer)

(let* ((mon (make-monochord '(6 5 4 3)))
       (m (render-monochord mon 25 0.5))
       (btikz (make-backend-tikz :filename "monochord.tex")))
  (draw-with-multiple-backends (list btikz) (list m))
  (compile-tikz btikz))
