(in-package :drawer)

(ql:quickload :vicentino-tunings)

(let* ((btikz (make-backend-tikz :filename "vicentino-bars-thirds.tex"))
       (int-terza-maggiore 5/4)
       (int-terza-minore 6/5)
       (int-terza-minore-mt (/ 6/5 (expt 81/80 1/4)))
       (int-i 11/9)
       (int-iia 60/49)
       (int-iib 49/40)
       (int-iii (* (sqrt 25/24) 6/5))
       (int-list (sort (copy-list (list (cons int-terza-maggiore "5:4")
                                        (cons int-terza-minore "6:5")
                                        (cons int-terza-minore-mt "IV ($\\frac{6}{5}\\frac{1}{\\sqrt[4]{\\frac{81}{80}}}$)")
                                        (cons int-i "I (11:9)")
                                        (cons int-iia "IIa (60:49)")
                                        (cons int-iib "IIb (49:40)")
                                        (cons int-iii "III ($\\frac{6}{5}\\sqrt{\\frac{25}{24}}$)")))
                       #'<
                       :key #'car))
       (bar-padding 3.5)
       (scale-factor 1/10)
       (label-padding 1)
       (bar-thickness 2.5)
       (bars-gr (gr (loop for len in int-list
                        for i from 0
                           collect
                           (let ((log-len (* scale-factor
                                             (vicentino-tunings:ratio->length (car len)))))
                             (gr (list (ln-shape (pt 0 (- (* i bar-padding)
                                                          (* 1/2 bar-thickness)))
                                           (list log-len bar-thickness (- log-len)))
                                       (make-text (format nil "~a" (cdr len))
                                                  (pt (+ log-len label-padding)
                                                      (* i bar-padding))
                                                  :h-align :left))))))))
  (draw-with-multiple-backends (list btikz) (list bars-gr))
  (compile-tikz btikz))

(let* ((btikz (make-backend-tikz :filename "vicentino-bars-thirds-major.tex"))
       (int-terza-maggiore 5/4)
       (int-quarta 4/3)
       (int-quarta-mt (* 4/3 (expt 81/80 1/4)))
       (int-i 9/7)
       (int-iia 40/31)
       (int-iib 31/24)
       (int-iii (* 5/4 (sqrt 16/15)))
       (int-list (sort (copy-list (list (cons int-terza-maggiore "5:4")
                                        (cons int-quarta "4:3")
                                        (cons int-quarta-mt "IV ($\\frac{4}{3}\\sqrt{\\frac{81}{80}}$)")
                                        (cons int-i "I (9:7)")
                                        (cons int-iia "IIa (40:31)")
                                        (cons int-iib "IIb (31:24)")
                                        (cons int-iii "III ($\\frac{5}{4}\\sqrt{\\frac{16}{15}}$)")))
                       #'<
                       :key #'car))
       (bar-padding 3.5)
       (scale-factor 1/10)
       (label-padding 1)
       (bar-thickness 2.5)
       (bars-gr (gr (loop for len in int-list
                        for i from 0
                           collect
                           (let ((log-len (* scale-factor
                                             (vicentino-tunings:ratio->length (car len)))))
                             (gr (list (ln-shape (pt 0 (- (* i bar-padding)
                                                          (* 1/2 bar-thickness)))
                                           (list log-len bar-thickness (- log-len)))
                                       (make-text (format nil "~a" (cdr len))
                                                  (pt (+ log-len label-padding)
                                                      (* i bar-padding))
                                                  :h-align :left))))))))
  (draw-with-multiple-backends (list btikz) (list bars-gr))
  (compile-tikz btikz))


(let* ((btikz (make-backend-tikz :filename "vicentino-bars-thirds-propinqui.tex"))
       (int-terza-minore-mt (vicentino-tunings:interval :tuning1 :c :up :e♭))
       (int-terza-minore-propinqua (vicentino-tunings:interval :tuning1 :c :up :Ė♭))
       (int-terza-minore-iii (* (sqrt 25/24) 6/5))
       (int-terza-maggiore (vicentino-tunings:interval :tuning1 :c :up :e))
       (int-terza-maggiore-propinqua (vicentino-tunings:interval :tuning1 :c :up :Ė))
       (int-terza-maggiore-iii (* 5/4 (sqrt 16/15)))
       (int-quarta-minima (vicentino-tunings:interval :tuning1 :c :up :e♯))
       (int-quarta-mt (vicentino-tunings:interval :tuning1 :c :up :f))
       (int-list (sort (copy-list (list (cons int-terza-minore-mt "terza minore [archicembalo]")
                                        (cons int-terza-minore-propinqua "terza più di minore [archicembalo]")
                                        (cons int-terza-minore-iii "terza più di minore [III]")
                                        (cons int-terza-maggiore "terza maggiore [archicembalo]")
                                        (cons int-terza-maggiore-propinqua "terza più di maggiore [archicembalo]")
                                        (cons int-terza-maggiore-iii "terza più di maggiore [III]")
                                        (cons int-quarta-minima "[quarta minima, archicembalo]")
                                        (cons int-quarta-mt "quarta [archicembalo]")))
                       #'<
                       :key #'car))
       (bar-padding 3.5)
       (scale-factor 1/10)
       (label-padding 1)
       (bar-thickness 2.5)
       (bars-gr (gr (loop for len in int-list
                        for i from 0
                           collect
                           (let ((log-len (* scale-factor
                                             (vicentino-tunings:ratio->length (car len)))))
                             (gr (list (ln-shape (pt 0 (- (* i bar-padding)
                                                          (* 1/2 bar-thickness)))
                                           (list log-len bar-thickness (- log-len)))
                                       (make-text (format nil "~a" (cdr len))
                                                  (pt (+ log-len label-padding)
                                                      (* i bar-padding))
                                                  :h-align :left))))))))
  (draw-with-multiple-backends (list btikz) (list bars-gr))
  (compile-tikz btikz))
