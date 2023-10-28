(in-package :drawer)

(ql:quickload :vicentino-tunings)

(let* ((btikz (make-backend-tikz :filename "vicentino-bars-thirds.tex"))
       (int-terza-maggiore 5/4)
       (int-terza-minore 6/5)
       (int-i 11/9)
       (int-iia 60/49)
       (int-iib 49/40)
       (int-iii (* (sqrt 25/24) 6/5))
       (int-list (sort (copy-list (list int-terza-maggiore
                                        int-terza-minore
                                        int-i
                                        int-iia
                                        int-iib
                                        int-iii))
                       #'<))
       (line-padding 3)
       (scale-factor 1/10)
       (label-padding 2)
       (lines-gr (gr (loop for len in int-list
                        for i from 0
                           collect
                           (let ((log-len (* scale-factor
                                             (vicentino-tunings:ratio->length len))))
                             (gr (list (ln (pt 0 (* i line-padding))
                                           (pt log-len (* i line-padding)))
                                       (make-text (format nil "~f" log-len)
                                                  (pt (+ log-len label-padding)
                                                      (* i line-padding))
                                                  :h-align :left))))))))
  (draw-with-multiple-backends (list btikz) (list lines-gr))
  (compile-tikz btikz))
