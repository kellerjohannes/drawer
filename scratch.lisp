(in-package :drawer)

(let* ((btikz (make-backend-tikz :filename "vicentino-arbore.tex"))
       (max-height 150)
       (y-base 5)
       (origin (pt 0 0))
       (vert-line (ln origin (pt 0 max-height)))
       (unisono 0)
       (comma 2.5)
       (diesis-minore 10)
       (semitono-minore 20)
       (semitono-maggiore 30)
       (tono 50)
       (terza-minore 80)
       (terza-maggiore 100)
       (quarta 130)
       (quinta 180)
       (interval-dot (circ 0 0 1 :style-update '(:fill :fill)))
       (gen-offset 30)
       (home-intervals (list unisono semitono-maggiore tono terza-minore terza-maggiore quarta quinta))
       (bundle-intervals (list comma (- comma) diesis-minore (- diesis-minore) semitono-minore (- semitono-minore) semitono-maggiore (- semitono-maggiore) tono (- tono)))
       (bundle (gr (mapcar (lambda (bundle-interval)
                             (ln origin (pt bundle-interval gen-offset)))
                           bundle-intervals)))
       (gen-1-intervals nil))
  (loop for hm in home-intervals
        do (loop for gn in bundle-intervals
                 do (push (+ hm gn) gen-1-intervals)))
  (remove-duplicates gen-1-intervals)
  (let ((gen-2-dots nil))
    (loop for dt in gen-1-intervals
          do (loop for dt2 in bundle-intervals
                   do (push (+ dt dt2) gen-2-dots)))
    (remove-duplicates gen-2-dots)
    (flet ((gen-drawer (int y)
             (gr (list (cp interval-dot origin (pt int y))
                       (cp vert-line origin (pt int 0))
                       (cp bundle origin (pt int y))))))
      (let ((gen-1 (gr (mapcar (lambda (i) (gen-drawer i y-base)) home-intervals)))
            (gen-2 (gr (mapcar (lambda (i) (gen-drawer i (+ y-base gen-offset))) gen-1-intervals)))
            (gen-3 (gr (mapcar (lambda (i) (cp interval-dot origin (pt i (+ y-base (* 2 gen-offset)))))
                               gen-2-dots))))
        (draw-with-multiple-backends (list btikz) (list gen-1 gen-2 gen-3))
        (compile-tikz btikz)))))
