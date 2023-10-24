(in-package :drawer)


(let* (
       (btikz-core (make-backend-tikz :filename "vicentino-arbore-core-vocal.tex"))
       (btikz-simple (make-backend-tikz :filename "vicentino-arbore-simple-vocal.tex"))
       (btikz-complex (make-backend-tikz :filename "vicentino-arbore-complex-vocal.tex"))
       ;; (max-width 150)
       (x-base 5)
       (origin (pt 0 0))
       ;; (vert-line (ln (pt max-width 0) origin))
       (unisono 0)
       (comma 5)
       (diesis-minore 10)
       (semitono-minore 20)
       (semitono-maggiore 30)
       (tono 50)
       (terza-minore 80)
       (terza-maggiore 100)
       (quarta 130)
       (quinta 180)
       (interval-dot (circ 0 0 0.65 :style-update '(:fill :fill)))
       (gen-offset 30)
       (home-intervals (list unisono semitono-maggiore tono terza-minore terza-maggiore quarta quinta))
       (bundle-intervals (list comma (- comma) diesis-minore (- diesis-minore) semitono-minore (- semitono-minore) semitono-maggiore (- semitono-maggiore) tono (- tono)))
       (bundle (gr (mapcar (lambda (bundle-interval)
                             (ln origin (pt gen-offset bundle-interval)))
                           bundle-intervals)))
       (bundle-labels (list "tono discendente" "semitono maggiore discendente"
                            "diesis enarmonico maggiore discendente / semitono minore discendente"
                            "diesis enarmonico minore discendente" "comma discendente"
                            "comma ascendente" "diesis enarmonico minore ascendente"
                            "diesis enarmonico maggiore ascendente / semitono minore ascendente"
                            "semitono maggiore ascendente" "tono ascendente"))
       (bundle-labels-gr (gr (mapcar (lambda (pos text)
                                       (make-text text (pt (+ gen-offset) pos) :h-align :left))
                                     (sort (copy-list bundle-intervals) #'<)
                                     bundle-labels)))
       (gen-1-intervals nil))
  (loop for hm in home-intervals
        do (loop for gn in bundle-intervals
                 do (push (+ hm gn) gen-1-intervals)))
  (remove-duplicates gen-1-intervals)
  (let ((gen-3-dots nil)
        (gen-2-dots nil))
    (loop for dt in home-intervals
          do (loop for dt2 in bundle-intervals
                   do (push (+ dt dt2) gen-2-dots)))
    (remove-duplicates gen-2-dots)
    (loop for dt in gen-1-intervals
          do (loop for dt2 in bundle-intervals
                   do (push (+ dt dt2) gen-3-dots)))
    (remove-duplicates gen-3-dots)
    (flet ((gen-drawer (int x)
             (gr (list ;;(cp interval-dot origin (pt x int))
                  ;;(cp vert-line origin (pt x int))
                  (cp bundle origin (pt x int))))))
      (let* ((gen-1 (gr (mapcar (lambda (i) (gen-drawer i x-base)) home-intervals)))
             (gen-1-dots-gr (gr (mapcar (lambda (i)
                                          (cp interval-dot origin (pt x-base i)))
                                        home-intervals)))
             (gen-2 (gr (mapcar (lambda (i)
                                  (gen-drawer i (+ x-base gen-offset)))
                                gen-1-intervals)))
             (gen-2-dots-gr (gr (mapcar (lambda (i)
                                          (cp interval-dot origin (pt (+ x-base gen-offset) i)))
                                        gen-1-intervals)))
             (gen-3 (gr (mapcar (lambda (i)
                                  (cp interval-dot origin (pt (+ x-base (* 2 gen-offset)) i)))
                                gen-3-dots)))
             (label-padding 2)
             (gen-1-labels (list "unisono" "semitono maggiore" "tono" "terza minore" "terza maggiore" "quarta" "quinta"))
             (gen-1-labels-gr (gr (mapcar (lambda (pos text)
                                            (make-text text (pt (- x-base label-padding) pos)
                                                       :h-align :right))
                                          home-intervals
                                          gen-1-labels)))
             (gen-2-labels (list nil nil nil nil nil nil "comma" "diesis enarmonico minore"
                                 "diesis enarmonico maggiore / semitono minore"
                                 "semitono maggiore propinquissimo (minuendo)" "semitono maggiore"
                                 "semitono maggiore propinquissimo (aggiugnendo)" "tono minore"
                                 "tono propinquissimo (minuendo)" "tono naturale / tono accidentale"
                                 "tono propinquissimo (aggiugnendo)" "tono maggiore"
                                 "terza manca di minore / terza minima / terza propinqua (minuendo)"
                                 "terza minore propinquissima (minuendo)"
                                 "terza minore naturale / terza minore accidentale"
                                 "terza minore propinquissima (aggiugnendo)"
                                 "terza più di minore / terza propinqua (aggiugnendo)"
                                 "terza maggiore propinquissima (minuendo)"
                                 "terza maggiore naturale / terza maggiore accidentale"
                                 "terza maggiore propinquissima (aggiugnendo)"
                                 "terza maggiore propinqua (aggiugnendo)"
                                 "[quarta minima / quarta propinqua (minuendo)]"
                                 "quarta propinquissima (minuendo)"
                                 "quarta naturale / quarta accidentale"
                                 "quarta propinquissima (aggiugnendo)" "salto della più di quarta"
                                 "tritono naturale / tritono accidentale"
                                 "quinta imperfetta naturale / quinta imperfetta accidentale"
                                 "salto della più di quinta imperfetta"
                                 "quinta propinquissima (minuendo)"
                                 "quinta naturale / quinta accidentale"
                                 "quinta propinquissima (aggiugnendo)" "salto della più di quinta"
                                 "[quinta maggiore]" "sesta minore" "sesta maggiore"))
             (gen-2-labels-gr (gr (mapcar (lambda (pos text)
                                            (make-text text (pt (+ x-base
                                                                   gen-offset
                                                                   label-padding)
                                                                pos)
                                                       :h-align :left))
                                          (sort (remove-duplicates gen-1-intervals) #'<)
                                          gen-2-labels)))
             (gen-3-labels (list nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
                                 "comma" "diesis enarmonico minore" "diesis enarmonico minore propinquissimo (aggiugnendo) / diesis maggiore propinquissimo (minuendo)"
                                 "diesis enarmonico maggiore / semitono minore"
                                 "semitono maggiore propinquissimo (minuendo)" "semitono maggiore"
                                 "semitono maggiore propinquissimo (aggiugnendo)" "tono minore"
                                 "tono minore propinquissimo (aggiugnendo) / tono propinquissimo (minuendo)" "tono naturale / tono accidentale"
                                 "tono propinquissimo (aggiugnendo)" "tono maggiore"
                                 "tono maggiore propinquissimo"
                                 "terza manca di minore / terza minima / terza propinqua (minuendo)"
                                 "terza minima propinquissima (aggiugnendo) / terza minore propinquissima (minuendo)"
                                 "terza minore naturale / terza minore accidentale"
                                 "terza minore propinquissima (aggiugnendo)"
                                 "terza più di minore / terza propinqua (aggiugnendo)"
                                 "terza maggiore propinquissima (minuendo)"
                                 "terza maggiore naturale / terza maggiore accidentale"
                                 "terza maggiore propinquissima (aggiugnendo)"
                                 "terza più di maggiore / terza propinqua (aggiugnendo)"
                                 "terza più di maggiore propinquissima (aggiugnendo)"
                                 "[quarta minima / quarta propinqua (minuendo)]"
                                 "[quarta minima propinquissima (aggiugnendo)] / quarta propinquissima (minuendo)"
                                 "quarta naturale / quarta accidentale"
                                 "quarta propinquissima (aggiugnendo)" "salto della più di quarta"
                                 "salto della più di quarta propinquissima"
                                 "tritono naturale / tritono accidentale"
                                 "tritono propinquissimo (aggiugnendo)"
                                 "quinta imperfetta naturale / quinta imperfetta accidentale"
                                 "quinta imperfetta propinquissima (aggiugnendo)"
                                 "salto della più di quinta imperfetta"
                                 "salto della più di quinta imperfetta propinquissima (aggiugnendo) / quinta propinquissima (minuendo)"
                                 "quinta naturale / quinta accidentale"
                                 "quinta propinquissima (aggiugnendo)" "salto della più di quinta"
                                 "salto della più di quinta propinquissimo (aggiugnendo)"
                                 "[quinta maggiore]"
                                 "[quinta maggiore propinquissima (aggiugnendo)"
                                 "sesta minore naturale / sesta minore accidentale"
                                 "sesta minore propinquissima (aggiugnendo)"
                                 "sesta minore propinqua (aggiugnendo)"
                                 "sesta minore propinqua (aggiugnendo) propinquissima (aggiugnendo) / sesta maggiore propinquissima (minuendo)"
                                 "sesta maggiore naturale / sesta maggiore accidentale"
                                 "sesta maggiore propinquissima (aggiugnendo)"
                                 "sesta maggiore propinqua (aggiugnendo)"
                                 "sesta maggiore propinqua (aggiugnendo) propinquissima (aggiugnendo)"
                                 "settima minore naturale / settima minore accidentale"
                                 "settima maggiore naturale / settima minore accidentale"))
             (gen-3-labels-gr (gr (mapcar (lambda (pos text)
                                            (make-text text (pt (+ x-base
                                                                   (* 2  gen-offset)
                                                                   label-padding)
                                                                pos)
                                                       :h-align :left))
                                          (sort (remove-duplicates gen-3-dots) #'<)
                                          gen-3-labels))))
        (draw-with-multiple-backends (list btikz-simple) (list gen-1 gen-1-dots-gr gen-1-labels-gr
                                                               gen-2-dots-gr gen-2-labels-gr))
        (draw-with-multiple-backends (list btikz-complex) (list gen-1 gen-1-dots-gr gen-1-labels-gr
                                                                gen-2 gen-2-dots-gr gen-3 gen-3-labels-gr))
        (draw-with-multiple-backends (list btikz-core) (list bundle bundle-labels-gr))
        (compile-tikz btikz-simple)
        (compile-tikz btikz-complex)
        (compile-tikz btikz-core)
        ))))






(let* (
       (btikz-core (make-backend-tikz :filename "vicentino-arbore-core-instrumental.tex"))
       (btikz-simple (make-backend-tikz :filename "vicentino-arbore-simple-instrumental.tex"))
       (btikz-complex (make-backend-tikz :filename "vicentino-arbore-complex-instrumental.tex"))
       ;; (max-width 150)
       (upscale 5/3)
       (x-base (* upscale 5))
       (origin (pt 0 0))
       ;; (vert-line (ln (pt max-width 0) origin))
       (unisono 0)
       (comma (* upscale 10/8))
       (diesis-minore (* upscale 10))
       (semitono-minore (* upscale 20))
       (semitono-maggiore (* upscale 30))
       (tono (* upscale 50))
       (terza-minore (* upscale 80))
       (terza-maggiore (* upscale 100))
       (quarta (* upscale 130))
       (quinta (* upscale 180))
       (interval-dot (circ 0 0 0.65 :style-update '(:fill :fill)))
       (gen-offset (* upscale 30))
       (home-intervals (list unisono semitono-maggiore tono terza-minore terza-maggiore quarta quinta))
       (bundle-intervals (list comma (- comma) diesis-minore (- diesis-minore) semitono-minore (- semitono-minore) semitono-maggiore (- semitono-maggiore) tono (- tono)))
       (bundle (gr (mapcar (lambda (bundle-interval)
                             (ln origin (pt gen-offset bundle-interval)))
                           bundle-intervals)))
       (bundle-labels (list "tono discendente" "semitono maggiore discendente"
                            "diesis enarmonico maggiore discendente / semitono minore discendente"
                            "diesis enarmonico minore discendente" "comma discendente"
                            "comma ascendente" "diesis enarmonico minore ascendente"
                            "diesis enarmonico maggiore ascendente / semitono minore ascendente"
                            "semitono maggiore ascendente" "tono ascendente"))
       (bundle-labels-gr (gr (mapcar (lambda (pos text)
                                       (make-text text (pt (+ gen-offset) pos) :h-align :left))
                                     (sort (copy-list bundle-intervals) #'<)
                                     bundle-labels)))
       (gen-1-intervals nil))
  (loop for hm in home-intervals
        do (loop for gn in bundle-intervals
                 do (push (+ hm gn) gen-1-intervals)))
  (remove-duplicates gen-1-intervals)
  (let ((gen-3-dots nil)
        (gen-2-dots nil))
    (loop for dt in home-intervals
          do (loop for dt2 in bundle-intervals
                   do (push (+ dt dt2) gen-2-dots)))
    (remove-duplicates gen-2-dots)
    (loop for dt in gen-1-intervals
          do (loop for dt2 in bundle-intervals
                   do (push (+ dt dt2) gen-3-dots)))
    (remove-duplicates gen-3-dots)
    (flet ((gen-drawer (int x)
             (gr (list ;;(cp interval-dot origin (pt x int))
                  ;;(cp vert-line origin (pt x int))
                  (cp bundle origin (pt x int))))))
      (let* ((gen-1 (gr (mapcar (lambda (i) (gen-drawer i x-base)) home-intervals)))
             (gen-1-dots-gr (gr (mapcar (lambda (i)
                                          (cp interval-dot origin (pt x-base i)))
                                        home-intervals)))
             (gen-2 (gr (mapcar (lambda (i)
                                  (gen-drawer i (+ x-base gen-offset)))
                                gen-1-intervals)))
             (gen-2-dots-gr (gr (mapcar (lambda (i)
                                          (cp interval-dot origin (pt (+ x-base gen-offset) i)))
                                        gen-1-intervals)))
             (gen-3 (gr (mapcar (lambda (i)
                                  (cp interval-dot origin (pt (+ x-base (* 2 gen-offset)) i)))
                                gen-3-dots)))
             (label-padding 2)
             (gen-1-labels (list "unisono" "semitono maggiore" "tono" "terza minore" "terza maggiore" "quarta" "quinta"))
             (gen-1-labels-gr (gr (mapcar (lambda (pos text)
                                            (make-text text (pt (- x-base label-padding) pos)
                                                       :h-align :right))
                                          home-intervals
                                          gen-1-labels)))
             (gen-2-labels (list nil nil nil nil nil nil "comma" "diesis enarmonico minore"
                                 "diesis enarmonico maggiore / semitono minore"
                                 "semitono maggiore propinquissimo (minuendo)" "semitono maggiore"
                                 "semitono maggiore propinquissimo (aggiugnendo)" "tono minore"
                                 "tono propinquissimo (minuendo)" "tono naturale / tono accidentale"
                                 "tono propinquissimo (aggiugnendo)" "tono maggiore"
                                 "terza manca di minore / terza minima / terza propinqua (minuendo)"
                                 "terza minore propinquissima (minuendo)"
                                 "terza minore naturale / terza minore accidentale"
                                 "terza minore propinquissima (aggiugnendo)"
                                 "terza più di minore / terza propinqua (aggiugnendo)"
                                 "terza maggiore propinquissima (minuendo)"
                                 "terza maggiore naturale / terza maggiore accidentale"
                                 "terza maggiore propinquissima (aggiugnendo)"
                                 "terza maggiore propinqua (aggiugnendo)"
                                 "[quarta minima / quarta propinqua (minuendo)]"
                                 "quarta propinquissima (minuendo)"
                                 "quarta naturale / quarta accidentale"
                                 "quarta propinquissima (aggiugnendo)" "salto della più di quarta"
                                 "tritono naturale / tritono accidentale"
                                 "quinta imperfetta naturale / quinta imperfetta accidentale"
                                 "salto della più di quinta imperfetta"
                                 "quinta propinquissima (minuendo)"
                                 "quinta naturale / quinta accidentale"
                                 "quinta propinquissima (aggiugnendo)" "salto della più di quinta"
                                 "[quinta maggiore]" "sesta minore" "sesta maggiore"))
             (gen-2-labels-gr (gr (mapcar (lambda (pos text)
                                            (make-text text (pt (+ x-base
                                                                   gen-offset
                                                                   label-padding)
                                                                pos)
                                                       :h-align :left))
                                          (sort (remove-duplicates gen-1-intervals) #'<)
                                          gen-2-labels)))
             (gen-3-labels (list nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
                                 nil nil nil nil
                                 "comma"
                                 "comma propimquissimo (aggiugnendo)"
                                 "diesis enarmonico minore propinquissimo (minuendo)"
                                 "diesis enarmonico minore"
                                 "diesis enarmonico minore propinquissimo (aggiugnendo)"
                                 "diesis enarmonico maggiore propinquissimo (minuendo)"
                                 "diesis enarmonico maggiore / semitono minore"
                                 "diesis enarmonico maggiore propinquissimo (aggiugnendo)"
                                 "semitono maggiore propinquissimo (minuendo) propinquissimo (minuendo)"
                                 "semitono maggiore propinquissimo (minuendo)"
                                 "semitono maggiore"
                                 "semitono maggiore propinquissimo (aggiugnendo)"
                                 "semitono maggiore propinquissimo (aggiugnendo) propinquissimo (aggiugnendo)"
                                 "tono minore propinquissimo (minuendo)"
                                 "tono minore"
                                 "tono minore propinquissimo (aggiugnendo)"
                                 "tono propinquissimo (minuendo) propinquissimo (minuendo)"
                                 "tono propinquissimo (minuendo)"
                                 "tono naturale / tono accidentale"
                                 "tono propinquissimo (aggiugnendo)"
                                 "tono propinquissimo (aggiugnendo) propinquissimo (aggiugnendo)"
                                 "tono maggiore propinquissimo (minuendo)"
                                 "tono maggiore"
                                 "tono maggiore propinquissimo"
                                 "terza minima propinquissima (minuendo)"
                                 "terza manca di minore / terza minima / terza propinqua (minuendo)"
                                 "terza minima propinquissima (aggiugnendo)"
                                 "terza minore propinquissima (minuendo) propinquissima (minuendo)"
                                 "terza minore propinquissima (minuendo)"
                                 "terza minore naturale / terza minore accidentale"
                                 "terza minore propinquissima (aggiugnendo)"
                                 "terza minore propinquissima (aggiugnendo) propinquissima (aggiugnendo)"
                                 "terza più di minore propinquissima (minuendo)"
                                 "terza più di minore / terza propinqua (aggiugnendo)"
                                 "terza più di minore propinquissima (aggiugnendo)"
                                 "terza maggiore propinquissima (minuendo) propinquissima (minuendo)"
                                 "terza maggiore propinquissima (minuendo)"
                                 "terza maggiore naturale / terza maggiore accidentale"
                                 "terza maggiore propinquissima (aggiugnendo)"
                                 "terza maggiore propinquissima (aggiugnendo) propinquissima (aggiugnendo)"
                                 "terza più di maggiore propinquissima (minuendo)"
                                 "terza più di maggiore / terza propinqua (aggiugnendo)"
                                 "terza più di maggiore propinquissima (aggiugnendo)"
                                 "[quarta minima propinquissima (minuendo)]"
                                 "[quarta minima / quarta propinqua (minuendo)]"
                                 "[quarta minima propinquissima (aggiugnendo)]"
                                 "quarta propinquissima (minuendo) propinquissima (minuendo)"
                                 "quarta propinquissima (minuendo)"
                                 "quarta naturale / quarta accidentale"
                                 "quarta propinquissima (aggiugnendo)"
                                 "quarta propinquissima (aggiugnendo) propinquissima (aggiugnendo)"
                                 "salto della più di quarta propinquissimo (minuendo)"
                                 "salto della più di quarta"
                                 "salto della più di quarta propinquissima"
                                 "tritono propinquissimo (minuendo)"
                                 "tritono naturale / tritono accidentale"
                                 "tritono propinquissimo (aggiugnendo)"
                                 "quinta imperfetta propinquissima (minuendo)"
                                 "quinta imperfetta naturale / quinta imperfetta accidentale"
                                 "quinta imperfetta propinquissima (aggiugnendo)"
                                 "salto della più di quinta imperfetta propinquissimo (minuendo)"
                                 "salto della più di quinta imperfetta"
                                 "salto della più di quinta imperfetta propinquissima (aggiugnendo)"
                                 "quinta naturale propinquissima (minuendo) propinquissima (minuendo)"
                                 "quinta naturale propinquissima (minuendo)"
                                 "quinta naturale / quinta accidentale"
                                 "quinta propinquissima (aggiugnendo)"
                                 "quinta propinquissima (aggiugnendo) propinquissima (aggiugnendo)"
                                 "salto della più di quinta propinquissimo (minuendo)"
                                 "salto della più di quinta"
                                 "salto della più di quinta propinquissimo (aggiugnendo)"
                                 "[quinta maggiore propinquissima (minuendo)]"
                                 "[quinta maggiore]"
                                 "[quinta maggiore propinquissima (aggiugnendo)"
                                 "sesta minore propinquissima (minuendo)"
                                 "sesta minore naturale / sesta minore accidentale"
                                 "sesta minore propinquissima (aggiugnendo)"
                                 "sesta minore propinqua (aggiugnendo)"
                                 "sesta maggiore propinquissima (minuendo)"
                                 "sesta maggiore naturale / sesta maggiore accidentale"
                                 "sesta maggiore propinquissima (aggiugnendo)"
                                 "sesta maggiore propinqua (aggiugnendo)"
                                 "settima minore propinqua (minuendo)"
                                 "settima minore naturale / settima minore accidentale"
                                 "settima maggiore naturale / settima maggiore accidentale"))
             (gen-3-labels-gr (gr (mapcar (lambda (pos text)
                                            (make-text text (pt (+ x-base
                                                                   (* 2  gen-offset)
                                                                   label-padding)
                                                                pos)
                                                       :h-align :left))
                                          (sort (remove-duplicates gen-3-dots) #'<)
                                          gen-3-labels))))
        (draw-with-multiple-backends (list btikz-simple) (list gen-1 gen-1-dots-gr gen-1-labels-gr
                                                               gen-2-dots-gr gen-2-labels-gr))
        (draw-with-multiple-backends (list btikz-complex) (list gen-1 gen-1-dots-gr gen-1-labels-gr
                                                                gen-2 gen-2-dots-gr gen-3 gen-3-labels-gr))
        (draw-with-multiple-backends (list btikz-core) (list bundle bundle-labels-gr))
        (compile-tikz btikz-simple)
        (compile-tikz btikz-complex)
        (compile-tikz btikz-core)
        ))))
