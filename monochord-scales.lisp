(in-package :drawer)

;; build up to ji
(let* ((mon (make-monochord '(2 1)))
       (string-length 200)
       (m-oct (render-monochord mon string-length 2.4 0.1 0.7 :up))
       (btikz (make-backend-tikz :filename "monochord-octave.tex")))
  (draw-with-multiple-backends (list btikz) (list m-oct))
  (compile-tikz btikz)

  (add-ratio mon 2/3 0)
  (let ((m-oct-fifth (render-monochord mon string-length 2.4 0.1 0.7 :up)))
    (setf btikz (make-backend-tikz :filename "monochord-octave-fifth.tex"))
    (draw-with-multiple-backends (list btikz) (list m-oct-fifth))
    (compile-tikz btikz))

  (add-ratio mon 4/5 0)
  (let ((m-oct-fifth-third (render-monochord mon string-length 2.4 0.1 0.7 :up)))
    (setf btikz (make-backend-tikz :filename "monochord-octave-fifth-third.tex"))
    (draw-with-multiple-backends (list btikz) (list m-oct-fifth-third))
    (compile-tikz btikz))


  (add-ratio mon 8/9 0)
  (add-ratio mon 9/8 3)
  (add-ratio mon 8/9 4)
  (add-ratio mon 9/10 5)
  (let ((m-ji (render-monochord mon string-length 2.4 0.1 0.7 :toggle)))
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
  (let ((m-pyth (render-monochord mon 200 2.4 0.1 0.7 :toggle))
        (btikz (make-backend-tikz :filename "monochord-pythagorean.tex")))
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

;; gaffurio 1492
(let ((mon (make-monochord '(9 12 16 18 24 36))))
  (let ((m-pyth (render-monochord mon 150 2.2 0.1 0.8 :up))
        (btikz (make-backend-tikz :filename "monochord-gaffurio-1492.tex")))
    (draw-with-multiple-backends (list btikz) (list m-pyth))
    (compile-tikz btikz)))

;; gaffurio 1492 II
(let ((mon (make-monochord '(192 216 243 256))))
  (let ((m-pyth (render-monochord mon 150 2.2 0.1 0.8 :up))
        (btikz (make-backend-tikz :filename "monochord-gaffurio-1492-b.tex")))
    (draw-with-multiple-backends (list btikz) (list m-pyth))
    (compile-tikz btikz)))

;; fogliano 1529
(let ((mon (make-monochord '(2 1))))
  (add-ratio mon 3/4 0)
  (add-ratio mon 2/3 0)
  (add-ratio mon 5/4 2)
  (add-ratio mon 4/5 3)
  (add-ratio mon 3/2 4)
  (add-ratio mon 25/15 5)
  (add-ratio mon 18/15 6)
  (add-ratio mon 3/2 6)
  (add-ratio mon 3/4 4)
  (add-ratio mon 9/8 7)
  (add-ratio mon 3/4 5)
  (add-ratio mon 5/8 2)
  (add-ratio mon 3/2 9)
  (let ((m-pyth (render-monochord mon 650 2.8 0.2 0.7 :toggle))
        (btikz (make-backend-tikz :filename "monochord-fogliano-1529.tex")))
    (draw-with-multiple-backends (list btikz) (list m-pyth))
    (compile-tikz btikz)))

;; zarlino p17
(let ((mon (make-monochord '(18 12 9 6))))
  (let ((m-pyth (render-monochord mon 80 2.2 0.1 0.8 :up))
        (btikz (make-backend-tikz :filename "monochord-zarlino-p17.tex")))
    (draw-with-multiple-backends (list btikz) (list m-pyth))
    (compile-tikz btikz)))

;; zarlino tolomeo
(let ((mon (make-monochord '(180 160 144 135 120 108 96 90))))
  (let ((m-pyth (render-monochord mon 300 2.2 0.1 0.8 :up))
        (btikz (make-backend-tikz :filename "monochord-zarlino-tolomeo.tex")))
    (draw-with-multiple-backends (list btikz) (list m-pyth))
    (compile-tikz btikz)))






;; playground

(let ((mon (make-monochord '(1))))
  (add-ratio mon 2/3 0)
  (add-ratio mon 3/4 0)
  (add-ratio mon 4/5 0)
  (add-ratio mon 6/5 2)
  (add-ratio mon 4/3 4)

  (let ((m-pyth (render-monochord mon 350 2.2 0.2 0.6 :up))
        (btikz (make-backend-tikz :filename "monochord-live-5-ji.tex")))
    (draw-with-multiple-backends (list btikz) (list m-pyth))
    (compile-tikz btikz)))
