(in-package :drawer)

(let* ((mon (make-monochord '(2 1)))
       (string-length 200)
       (m-oct (render-monochord mon string-length 1))
       (btikz (make-backend-tikz :filename "monochord-octave.tex")))
  (draw-with-multiple-backends (list btikz) (list m-oct))
  (compile-tikz btikz)

  (add-ratio mon 2/3 0)
  (let ((m-oct-fifth (render-monochord mon string-length 1)))
    (setf btikz (make-backend-tikz :filename "monochord-octave-fifth.tex"))
    (draw-with-multiple-backends (list btikz) (list m-oct-fifth))
    (compile-tikz btikz))

  (add-ratio mon 4/5 0)
  (let ((m-oct-fifth-third (render-monochord mon string-length 1)))
    (setf btikz (make-backend-tikz :filename "monochord-octave-fifth-third.tex"))
    (draw-with-multiple-backends (list btikz) (list m-oct-fifth-third))
    (compile-tikz btikz))


  (add-ratio mon 8/9 0)
  (add-ratio mon 9/8 3)
  (add-ratio mon 8/9 4)
  (add-ratio mon 9/10 5)
  (let ((m-ji (render-monochord mon string-length 1)))
    (setf btikz (make-backend-tikz :filename "monochord-ji.tex"))
    (draw-with-multiple-backends (list btikz) (list m-ji))
    (compile-tikz btikz))

  )
