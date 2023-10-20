(in-package :drawer)

;; build up to ji
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
  (let ((m-ji (render-monochord mon string-length 2)))
    (setf btikz (make-backend-tikz :filename "monochord-ji.tex"))
    (draw-with-multiple-backends (list btikz) (list m-ji))
    (compile-tikz btikz)))

;; pythagorean
(let ((mon (make-monochord '(2 1))))
  (add-ratio mon 8/9 0)
  (add-ratio mon 8/9 1)
  (add-ratio mon 3/4 0)
  (add-ratio mon 2/3 0)
  (add-ratio mon 8/9 4)
  (add-ratio mon 8/9 5)
  (add-ratio mon 9/8 7)
  (let ((m-pyth (render-monochord mon 200 2))
        (btikz (make-backend-tikz :filename "monochord-pythagorean.tex")))
    (draw-with-multiple-backends (list btikz) (list m-pyth))
    (compile-tikz btikz)))


;; playground
(let ((mon (make-monochord '(1))))
  (add-ratio mon 3/4 0)
  (add-ratio mon 4/5 0)
  (add-ratio mon 5/6 0)
  (add-ratio mon 2/3 0)
  (add-ratio mon 1/2 0)

  (let ((m-pyth (render-monochord mon 250 2.2 0.2 0.6 :up))
        (btikz (make-backend-tikz :filename "monochord-live.tex")))
    (draw-with-multiple-backends (list btikz) (list m-pyth))
    (compile-tikz btikz)))

;; salinas diatonisch

(let ((mon (make-monochord '(144 135 120 108 96 90 81 80 72))))
  (let ((m-salinas (render-monochord mon 350 2.2 0.2 0.6 :toggle))
        (btikz (make-backend-tikz :filename "monochord-salinas-diat.tex")))
    (draw-with-multiple-backends (list btikz) (list m-salinas))
    (compile-tikz btikz)))

;; salinas chromatisch

(let ((mon (make-monochord '(2880 2700 2592 2560 2400 2304 2160 2025 2000 1920 1800 1728 1620 1600 1500 1440))))
  (let ((m-salinas (render-monochord mon 650 2.2 0.2 0.6 :toggle))
        (btikz (make-backend-tikz :filename "monochord-salinas-chrom.tex")))
    (draw-with-multiple-backends (list btikz) (list m-salinas))
    (compile-tikz btikz)))

;; salinas enharmonisch
(let ((mon (make-monochord '(57600 55296 54000 51840 51200 50625 50000 48000 46080 45000 43200 41472 40960 40500 40000 38400 36864 36000 34560 33750 32400 32000 30720 30000 28800))))
  (let ((m-pyth (render-monochord mon 850 2.2 0.2 0.6 :toggle))
        (btikz (make-backend-tikz :filename "monochord-salinas-enh.tex")))
    (draw-with-multiple-backends (list btikz) (list m-pyth))
    (compile-tikz btikz)))
