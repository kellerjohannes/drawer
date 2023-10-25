(in-package :drawer)

(ql:quickload :vicentino-tunings)

(defun remove-similar (num-list &optional (margin 0.1))
  (do* ((num-arr (make-array (length num-list)
                             :initial-contents (sort (copy-list num-list) #'<)))
        (i 0 (1+ i))
        (this-el (aref num-arr i))
        (next-el (aref num-arr (1+ i)))
        (result nil)
        (group nil))
       ((= i (length num-arr)) (reverse (mapcar (lambda (similars) (average similars)) result)))
    (let ((this-el (aref num-arr i))
          (next-el (when (< (1+ i) (length num-arr)) (aref num-arr (1+ i)))))
      (cond ((and next-el
                  (> this-el (- next-el margin))
                  (< this-el (+ next-el margin)))
             (push this-el group))
            (t (push this-el group)
               (push (copy-list group) result)
               (setf group nil))))))

;; 1/4 comma meantone, diatonic scale
(let* ((upscale 1)
       (note-scale '((:c . -1) (:d . 0) (:e . 0) (:f . 0) (:g . 0) (:a . 0) (:b♮ . 0)
                     (:c . 0) (:d . 1) (:e . 1) (:f . 1) (:g . 1) (:a . 1) (:b♮ . 1) (:c . 1)))
       (tick-positions (mapcar (lambda (id)
                                 (* upscale 0.1
                                    (+ (* (cdr id) 1200)
                                       (vicentino-tunings:interval-size :tuning1 :c :up (car id)))))
                               note-scale))
       (tick-labels (mapcar (lambda (id) (format nil "~a" (car id))) note-scale))
       (label-padding (* upscale 2))
       (origin (pt 0 0))
       (tick (circ 0 0 (* upscale 0.2) :style-update '(:fill :fill)))
       (ticks-gr (gr (mapcar (lambda (x) (cp tick origin (pt x 0))) tick-positions)))
       (tick-labels-gr (gr (mapcar (lambda (x text) (make-text text (pt x (- label-padding))))
                                   tick-positions
                                   tick-labels)))
       (size-list nil)
       (crossings-gr nil)
       (arcs-gr (gr (do ((remainder tick-positions (rest remainder))
                         (result nil))
                        ((null remainder) (progn (setf size-list (remove-similar size-list))
                                                 result))
                      (loop for el in (rest remainder)
                            do (let* ((a (pt (first remainder) 0))
                                      (b (pt el 0)))
                                 (push (distance-between-points a b) size-list)
                                 (push (circ (+ (value (x a))
                                                (* 1/2 (distance-between-points a b)))
                                             (* 1/2 (distance-between-points a b))
                                             (* upscale .09))
                                       crossings-gr)
                                 (push (arc a (* 1/2 (distance-between-points a b)) 180 0
                                            :mode :point)
                                       result))))))
       (tangent-start (* upscale -2))
       (tangent-end (+ (car (last tick-positions)) (* upscale 2)))
       (tangents-gr (gr (mapcar (lambda (y) (ln (pt tangent-start (* 1/2 y))
                                                (pt tangent-end (* 1/2 y))
                                                :style-update '(:line-type :dotted)))
                                (remove-duplicates size-list))))
       (interval-label-x (+ tangent-end (* upscale 1)))
       (interval-labels '("semitono maggiore" "tono naturale" "terza minore naturale"
                          "terza maggiore naturale" "quarta naturale" "tritono naturale"
                          "quinta imperfetta naturale" "quinta naturale" "sesta minore naturale"
                          "sesta maggiore naturale" "settima minore naturale"
                          "settima maggiore naturale" "ottava"))
       (interval-labels-gr (gr (mapcar (lambda (yy text)
                                         (make-text text
                                                    (pt interval-label-x (* 1/2 yy))
                                                    :h-align :left))
                                       (sort (copy-list (remove-similar size-list))
                                             #'<)
                                       interval-labels)))
       (btikz (make-backend-tikz :filename "vicentino-map-diatonic.tex")))
  (draw-with-multiple-backends (list btikz) (list ticks-gr tick-labels-gr
                                                  arcs-gr
                                                  tangents-gr
                                                  (gr crossings-gr)
                                                  interval-labels-gr))
  (compile-tikz btikz))


;; 1/4-comma, 19 per octave
(let* ((upscale 3)
       (note-scale '((:c . -1) (:c♯ . 0) (:d♭ . 0)
                     (:d . 0) (:d♯ . 0) (:e♭ . 0)
                     (:e . 0) (:e♯ . 0)
                     (:f . 0) (:f♯ . 0) (:g♭ . 0)
                     (:g . 0) (:g♯ . 0) (:a♭ . 0)
                     (:a . 0) (:a♯ . 0) (:b♭ . 0)
                     (:b♮ . 0) (:b♯ . 0)
                     (:c . 0) (:c♯ . 1) (:d♭ . 1)
                     (:d . 1) (:d♯ . 1) (:e♭ . 1)
                     (:e . 1) (:e♯ . 1)
                     (:f . 1) (:f♯ . 1) (:g♭ . 1)
                     (:g . 1) (:g♯ . 1) (:a♭ . 1)
                     (:a . 1) (:a♯ . 1) (:b♭ . 1)
                     (:b♮ . 1) (:b♯ . 1)
                     (:c . 1)
                     ))
       (tick-positions (mapcar (lambda (id)
                                 (* upscale 0.1
                                    (+ (* (cdr id) 1200)
                                       (vicentino-tunings:interval-size :tuning1 :c :up (car id)))))
                               note-scale))
       (tick-labels (mapcar (lambda (id) (format nil "~a" (car id))) note-scale))
       (label-padding (* upscale 1))
       (origin (pt 0 0))
       (tick (circ 0 0 (* upscale 0.2) :style-update '(:fill :fill)))
       (ticks-gr (gr (mapcar (lambda (x) (cp tick origin (pt x 0))) tick-positions)))
       (tick-labels-gr (gr (mapcar (lambda (x text) (make-text text (pt x (- label-padding))))
                                   tick-positions
                                   tick-labels)))
       (size-list nil)
       (crossings-gr nil)
       (arcs-gr (gr (do ((remainder tick-positions (rest remainder))
                         (result nil))
                        ((null remainder) (progn (remove-similar size-list)
                                                 result))
                      (loop for el in (rest remainder)
                            do (let* ((a (pt (first remainder) 0))
                                      (b (pt el 0)))
                                 (push (distance-between-points a b) size-list)
                                 (push (circ (+ (value (x a))
                                                (* 1/2 (distance-between-points a b)))
                                             (* 1/2 (distance-between-points a b))
                                             (* upscale .09))
                                       crossings-gr)
                                 (push (arc a (* 1/2 (distance-between-points a b)) 180 0
                                            :mode :point)
                                       result))))))
       (tangent-start (* upscale -2))
       (tangent-end (+ (car (last tick-positions)) (* upscale 2)))
       (tangents-gr (gr (mapcar (lambda (y) (ln (pt tangent-start (* 1/2 y))
                                                (pt tangent-end (* 1/2 y))
                                                :style-update '(:line-type :dotted)))
                                (remove-duplicates size-list))))
       (interval-label-x (+ tangent-end (* upscale 1)))
       (interval-labels '("diesis enarmonico minore"
                          "semitono minore"
                          "semitono maggiore"
                          nil
                          "tono minore [zwei Stimmungsvarianten]"
                          "tono naturale / tono accidentale"
                          "tono maggiore"
                          "terza manca di minore / terza minima"
                          "terza minore naturale / terza minore accidentale"
                          nil
                          "terza più di minore / terza propinqua (aggiugnendo) [zwei Stimmungsvarianten]"
                          "terza maggiore naturale / terza maggiore accidentale"
                          "terza più di maggiore / terza propinqua (aggiugnendo)"
                          "[quarta minima]"
                          "quarta naturale / quarta accidentale"
                          nil
                          "salto della più di quarta / quarta propinqua (aggiugnendo) [zwei Stimmungsvarianten]"
                          "tritono naturale"
                          "quinta imperfetta naturale"
                          nil
                          "salto della più di quinta imperfetta / quinta imperfetta propinqua (aggiugnendo) [zwei Stimmungsvarianten]"
                          "quinta naturale"))
       (interval-labels-gr (gr (mapcar (lambda (yy text)
                                         (make-text text
                                                    (pt interval-label-x (* 1/2 yy))
                                                    :h-align :left))
                                       (sort (copy-list (remove-similar size-list))
                                             #'<)
                                       interval-labels)))
       (btikz (make-backend-tikz :filename "vicentino-map-19.tex")))
  (draw-with-multiple-backends (list btikz) (list ticks-gr tick-labels-gr
                                                  arcs-gr
                                                  tangents-gr
                                                  (gr crossings-gr)
                                                  interval-labels-gr))
  (compile-tikz btikz))




;; 1/4-comma, 31 per octave
(let* ((upscale 3)
       (note-scale '((:c . -1) (:ċ . 0) (:c♯ . 0) (:d♭ . 0) (:ḋ♭ . 0)
                     (:d . 0) (:ḋ . 0) (:d♯ . 0) (:e♭ . 0) (:ė♭ . 0)
                     (:e . 0) (:ė . 0) (:e♯ . 0)
                     (:f . 0) (:ḟ . 0) (:f♯ . 0) (:g♭ . 0) (:ġ♭ . 0)
                     (:g . 0) (:ġ . 0) (:g♯ . 0) (:a♭ . 0) (:ȧ♭ . 0)
                     (:a . 0) (:ȧ . 0) (:a♯ . 0) (:b♭ . 0) (:ḃ♭ . 0)
                     (:b♮ . 0) (:ḃ♮ . 0) (:b♯ . 0)
                     (:c . 0) (:ċ . 1) (:c♯ . 1) (:d♭ . 1) (:ḋ♭ . 1)
                     (:d . 1) (:ḋ . 1) (:d♯ . 1) (:e♭ . 1) (:ė♭ . 1)
                     (:e . 1) (:ė . 1) (:e♯ . 1)
                     (:f . 1) (:ḟ . 1) (:f♯ . 1) (:g♭ . 1) (:ġ♭ . 1)
                     (:g . 1) (:ġ . 1) (:g♯ . 1) (:a♭ . 1) (:ȧ♭ . 1)
                     (:a . 1) (:ȧ . 1) (:a♯ . 1) (:b♭ . 1) (:ḃ♭ . 1)
                     (:b♮ . 1) (:ḃ♮ . 1) (:b♯ . 1)
                     (:c . 1)
                     ))
       (tick-positions (mapcar (lambda (id)
                                 (* upscale 0.1
                                    (+ (* (cdr id) 1200)
                                       (vicentino-tunings:interval-size :tuning1 :c :up (car id)))))
                               note-scale))
       (tick-labels (mapcar (lambda (id) (format nil "~a" (car id))) note-scale))
       (label-padding (* upscale 2))
       (origin (pt 0 0))
       (tick (circ 0 0 (* upscale 0.2) :style-update '(:fill :fill)))
       (ticks-gr (gr (mapcar (lambda (x) (cp tick origin (pt x 0))) tick-positions)))
       (tick-labels-gr (gr (mapcar (lambda (x text) (make-text text (pt x (- label-padding))))
                                   tick-positions
                                   tick-labels)))
       (size-list nil)
       (crossings-gr nil)
       (arcs-gr (gr (do ((remainder tick-positions (rest remainder))
                         (result nil))
                        ((null remainder) result)
                      (loop for el in (rest remainder)
                            do (let* ((a (pt (first remainder) 0))
                                      (b (pt el 0)))
                                 (push (distance-between-points a b) size-list)
                                 (push (circ (+ (value (x a))
                                                (* 1/2 (distance-between-points a b)))
                                             (* 1/2 (distance-between-points a b))
                                             (* upscale .09))
                                       crossings-gr)
                                 (push (arc a (* 1/2 (distance-between-points a b)) 180 0
                                            :mode :point)
                                       result))))))
       (tangent-start (* upscale -2))
       (tangent-end (+ (car (last tick-positions)) (* upscale 2)))
       (tangents-gr (gr (mapcar (lambda (y) (ln (pt tangent-start (* 1/2 y))
                                                (pt tangent-end (* 1/2 y))
                                                :style-update '(:line-type :dotted)))
                                (remove-duplicates size-list))))
       (btikz (make-backend-tikz :filename "vicentino-map-31.tex")))
  (draw-with-multiple-backends (list btikz) (list ticks-gr tick-labels-gr
                                                  arcs-gr
                                                  tangents-gr
                                                  (gr crossings-gr)))
  (compile-tikz btikz))





;; 1/4-comma, 31 per octave, plus sesto ordine
(let* ((upscale 10)
       (note-selection '((:c . -1) (:ċ . 0) (:c♯ . 0) (:d♭ . 0) (:ḋ♭ . 0)
                         (:d . 0) (:ḋ . 0) (:d♯ . 0) (:e♭ . 0) (:ė♭ . 0)
                         (:e . 0) (:ė . 0) (:e♯ . 0)
                         (:f . 0) (:ḟ . 0) (:f♯ . 0) (:g♭ . 0) (:ġ♭ . 0)
                         (:g . 0) (:ġ . 0) (:g♯ . 0) (:a♭ . 0) (:ȧ♭ . 0)
                         (:a . 0) (:ȧ . 0) (:a♯ . 0) (:b♭ . 0) (:ḃ♭ . 0)
                         (:b♮ . 0) (:ḃ♮ . 0) (:b♯ . 0)

                         (:dʼ . 0) (:eʼ . 0) (:gʼ . 0) (:aʼ . 0) (:b♮ʼ . 0)

                         (:c . 0) (:ċ . 1) (:c♯ . 1) (:d♭ . 1) (:ḋ♭ . 1)
                         (:d . 1) (:ḋ . 1) (:d♯ . 1) (:e♭ . 1) (:ė♭ . 1)
                         (:e . 1) (:ė . 1) (:e♯ . 1)
                         (:f . 1) (:ḟ . 1) (:f♯ . 1) (:g♭ . 1) (:ġ♭ . 1)
                         (:g . 1) (:ġ . 1) (:g♯ . 1) (:a♭ . 1) (:ȧ♭ . 1)
                         (:a . 1) (:ȧ . 1) (:a♯ . 1) (:b♭ . 1) (:ḃ♭ . 1)
                         (:b♮ . 1) (:ḃ♮ . 1) (:b♯ . 1)
                         (:c . 1)

                         (:dʼ . 1) (:eʼ . 1) (:gʼ . 1) (:aʼ . 1) (:b♮ʼ . 1)
                         ))
       (note-scale (sort (copy-list note-selection)
                         #'<
                         :key (lambda (id)
                                (+ (* (cdr id) 1200)
                                   (vicentino-tunings:interval-size :tuning1 :c :up (car id))))))
       (tick-positions (mapcar (lambda (id)
                                       (* upscale 0.1
                                          (+ (* (cdr id) 1200)
                                             (vicentino-tunings:interval-size :tuning1 :c :up (car id)))))
                                     note-scale))
       (tick-labels (mapcar (lambda (id) (format nil "~a" (car id))) note-scale))
       (label-padding (* upscale 0.7))
       (origin (pt 0 0))
       (tick (circ 0 0 (* upscale 0.2) :style-update '(:fill :fill)))
       (ticks-gr (gr (mapcar (lambda (x) (cp tick origin (pt x 0))) tick-positions)))
       (tick-labels-gr (gr (mapcar (lambda (x text) (make-text text (pt x (- label-padding))))
                                   tick-positions
                                   tick-labels)))
       (size-list nil)
       (crossings-gr nil)
       (arcs-gr (gr (do ((remainder tick-positions (rest remainder))
                         (result nil))
                        ((null remainder) (progn (setf size-list (remove-similar size-list 0.8))
                                                 result))
                      (loop for el in (rest remainder)
                            do (let* ((a (pt (first remainder) 0))
                                      (b (pt el 0)))
                                 (push (distance-between-points a b) size-list)
                                 (push (circ (+ (value (x a))
                                                (* 1/2 (distance-between-points a b)))
                                             (* 1/2 (distance-between-points a b))
                                             (* upscale .09))
                                       crossings-gr)
                                 (push (arc a (* 1/2 (distance-between-points a b)) 180 0
                                            :mode :point)
                                       result))))))
       (tangent-start (* upscale -2))
       (tangent-end (+ (car (last tick-positions)) (* upscale 2)))
       (tangents-gr (gr (mapcar (lambda (y) (ln (pt tangent-start (* 1/2 y))
                                                (pt tangent-end (* 1/2 y))
                                                :style-update '(:line-type :dotted)))
                                (remove-duplicates size-list))))
       (interval-label-x (+ tangent-end 1))
       (interval-labels '("comma"
                          "diesis enarmonico minore propinquissimo (minuendo)"
                          "diesis enarmonico minore I"
                          "diesis enarmonico minore II"
                          "diesis enarmonico minore propinquissimo (aggiugnendo)"
                          "semitono minore propinquissimo (minuendo)"
                          "semitono minore I"
                          "semitono minore II / semitono minore propinquissimo (aggiugnendo) [klanglich nicht unterscheidbar]"
                          "semitono maggiore propinquissimo (minuendo)"
                          "semitono maggiore I"
                          "semitono maggiore II / semitono maggiore propinquissimo (aggiugnendo) [klanglich nicht unterscheidbar]"

                          "tono minore propinquissimo (minuendo)"
                          "tono minore I"
                          "tono minore II"
                          "tono minore propinquissimo (aggiugnendo)"

                          "tono propinquissimo (minuendo)"
                          "tono I"
                          "tono II / tono propinquissimo (aggiugnendo) [klanglich nicht unterscheidbar]"

                          "tono maggiore propinquissimo (minuendo)"
                          "tono maggiore I"
                          "tono maggiore II"
                          "tono maggiore propinquissimo (aggiugnendo)"

                          "terza minima propinquissima (minuendo)"
                          "terza minima I"
                          "terza minima II"
                          "terza minima propinquissima (aggiugnendo)"

                          "terza minore propinquissima (minuendo)"
                          "terza minore I"
                          "terza minore II / terza minore propinquissima (aggiugnendo) [klanglich nicht unterscheidbar]"

                          "terza più di minore propinquissima (minuendo)"
                          "terza più di minore I"
                          "terza più di minore II"
                          "terza più di minore propinquissima (aggiugnendo)"

                          "terza maggiore propinquissima (minuendo)"
                          "terza maggiore I"
                          "terza maggiore II / terza maggiore propinquissima (aggiugnendo)"

                          "terza più di maggiore propinquissima (minuendo)"
                          "terza più di maggiore I"
                          "terza più di maggiore II / terza più di maggiore propinquissima (aggiugnendo)"

                          "[quarta minima propinquissima (minuendo)]"
                          "[quarta minima I]"
                          "[quarta minima II]"
                          "[quarta minima propinquissima (aggiugnendo)"

                          "quarta propinquissima (minuendo)"
                          "quarta I"
                          "quarta II / quarta propinquissima [klanglich nicht unterscheidbar]"

                          "salto della più di quarta, propinquissima (minuendo)"
                          "salto della più di quarta I"
                          "salto della più di quarta II"
                          "salto della più di quarta, propinquissima (aggiugnendo)"

                          "tritono propinquissimo (minuendo)"
                          "tritono I"
                          "tritono II / tritono propinquissimo (aggiugnendo) [klanglich nicht unterscheidbar]"

                          "quinta imperfetta propinquissima (minuendo)"
                          "quinta imperfetta I"
                          "quinta imperfetta II / quinta imperfetta propinquissima (aggiugnendo) [klanglich nicht unterscheidbar"

                          "salto della più di quinta imperfetta propinquissimo (minuendo)"
                          "salto della più di quinta imperfetta I"
                          "salto della più di quinta imperfetta II"
                          "salto della più di quinta imperfetta propinquissimo (aggiugnendo)"

                          "quinta propinquissima (minuendo)"
                          "quinta I"
                          "quinta II / quinta propinquissima (aggiugnendo)"))

       (interval-labels-gr (gr (mapcar (lambda (yy text)
                                         (make-text text
                                                    (pt interval-label-x (* 1/2 yy))
                                                    :h-align :left))
                                       (sort (copy-list (remove-similar size-list))
                                             #'<)
                                       interval-labels)))
       (btikz (make-backend-tikz :filename "vicentino-map-31-sesto.tex")))
  (draw-with-multiple-backends (list btikz) (list ticks-gr tick-labels-gr
                                                  arcs-gr
                                                  tangents-gr
                                                  (gr crossings-gr)
                                                  interval-labels-gr))
  (compile-tikz btikz))




;; 1/4-comma, 31 per octave, plus sesto ordine, plus hypothetical notes
(let* ((upscale 4)
       (note-selection '((:c . -1) (:ċ . 0) (:c♯ . 0) (:d♭ . 0) (:ḋ♭ . 0)
                         (:d . 0) (:ḋ . 0) (:d♯ . 0) (:e♭ . 0) (:ė♭ . 0)
                         (:e . 0) (:ė . 0) (:e♯ . 0)
                         (:f . 0) (:ḟ . 0) (:f♯ . 0) (:g♭ . 0) (:ġ♭ . 0)
                         (:g . 0) (:ġ . 0) (:g♯ . 0) (:a♭ . 0) (:ȧ♭ . 0)
                         (:a . 0) (:ȧ . 0) (:a♯ . 0) (:b♭ . 0) (:ḃ♭ . 0)
                         (:b♮ . 0) (:ḃ♮ . 0) (:b♯ . 0)

                         (:dʼ . 0) (:eʼ . 0) (:gʼ . 0) (:aʼ . 0) (:b♮ʼ . 0)
                         (:cʼ . 0) (:fʼ . 0) (:b♭ʼ . 0) (:c-ʼ . -1)

                         (:c . 0) (:ċ . 1) (:c♯ . 1) (:d♭ . 1) (:ḋ♭ . 1)
                         (:d . 1) (:ḋ . 1) (:d♯ . 1) (:e♭ . 1) (:ė♭ . 1)
                         (:e . 1) (:ė . 1) (:e♯ . 1)
                         (:f . 1) (:ḟ . 1) (:f♯ . 1) (:g♭ . 1) (:ġ♭ . 1)
                         (:g . 1) (:ġ . 1) (:g♯ . 1) (:a♭ . 1) (:ȧ♭ . 1)
                         (:a . 1) (:ȧ . 1) (:a♯ . 1) (:b♭ . 1) (:ḃ♭ . 1)
                         (:b♮ . 1) (:ḃ♮ . 1) (:b♯ . 1)
                         (:c . 1)

                         (:dʼ . 1) (:eʼ . 1) (:gʼ . 1) (:aʼ . 1) (:b♮ʼ . 1)
                         (:cʼ . 1) (:fʼ . 1) (:b♭ʼ . 1) (:c-ʼ . 0)
                         ))
       (note-scale (sort (copy-list note-selection)
                         #'<
                         :key (lambda (id)
                                (+ (* (cdr id) 1200)
                                   (vicentino-tunings:interval-size :tuning1
                                                                    :c
                                                                    :up
                                                                    (car id))))))
       (tick-positions (mapcar (lambda (id)
                                       (* upscale 0.1
                                          (+ (* (cdr id) 1200)
                                             (vicentino-tunings:interval-size :tuning1 :c :up (car id)))))
                                     note-scale))
       (tick-labels (mapcar (lambda (id) (format nil "~a" (car id))) note-scale))
       (label-padding (* upscale 2))
       (origin (pt 0 0))
       (tick (circ 0 0 (* upscale 0.2) :style-update '(:fill :fill)))
       (ticks-gr (gr (mapcar (lambda (x) (cp tick origin (pt x 0))) tick-positions)))
       (tick-labels-gr (gr (mapcar (lambda (x text) (make-text text (pt x (- label-padding))))
                                   tick-positions
                                   tick-labels)))
       (size-list nil)
       (crossings-gr nil)
       (arcs-gr (gr (do ((remainder tick-positions (rest remainder))
                         (result nil))
                        ((null remainder) result)
                      (loop for el in (rest remainder)
                            do (let* ((a (pt (first remainder) 0))
                                      (b (pt el 0)))
                                 (push (distance-between-points a b) size-list)
                                 (push (circ (+ (value (x a))
                                                (* 1/2 (distance-between-points a b)))
                                             (* 1/2 (distance-between-points a b))
                                             (* upscale .09))
                                       crossings-gr)
                                 (push (arc a (* 1/2 (distance-between-points a b)) 180 0
                                            :mode :point)
                                       result))))))
       (tangent-start (* upscale -2))
       (tangent-end (+ (car (last tick-positions)) (* upscale 2)))
       (tangents-gr (gr (mapcar (lambda (y) (ln (pt tangent-start (* 1/2 y))
                                                (pt tangent-end (* 1/2 y))
                                                :style-update '(:line-type :dotted)))
                                (remove-duplicates size-list))))
       (btikz (make-backend-tikz :filename "vicentino-map-31-sesto-plus.tex")))
  (draw-with-multiple-backends (list btikz) (list ticks-gr tick-labels-gr
                                                  arcs-gr
                                                  tangents-gr
                                                  (gr crossings-gr)))
  (compile-tikz btikz))





;; 1/4-comma, 31 per octave, plus sesto ordine
(let* ((upscale 4)
       (note-selection '((:c . -1) (:ċ . 0) (:c♯ . 0) (:d♭ . 0) (:ḋ♭ . 0)
                         (:d . 0) (:ḋ . 0) (:d♯ . 0) (:e♭ . 0) (:ė♭ . 0)
                         (:e . 0) (:ė . 0) (:e♯ . 0)
                         (:f . 0) (:ḟ . 0) (:f♯ . 0) (:g♭ . 0) (:ġ♭ . 0)
                         (:g . 0) (:ġ . 0) (:g♯ . 0) (:a♭ . 0) (:ȧ♭ . 0)
                         (:a . 0) (:ȧ . 0) (:a♯ . 0) (:b♭ . 0) (:ḃ♭ . 0)
                         (:b♮ . 0) (:ḃ♮ . 0) (:b♯ . 0)

                         (:dʼ . 0) (:eʼ . 0) (:gʼ . 0) (:aʼ . 0) (:b♮ʼ . 0)

                         (:c . 0) (:ċ . 1) (:c♯ . 1) (:d♭ . 1) (:ḋ♭ . 1)
                         (:d . 1) (:ḋ . 1) (:d♯ . 1) (:e♭ . 1) (:ė♭ . 1)
                         (:e . 1) (:ė . 1) (:e♯ . 1)
                         (:f . 1) (:ḟ . 1) (:f♯ . 1) (:g♭ . 1) (:ġ♭ . 1)
                         (:g . 1) (:ġ . 1) (:g♯ . 1) (:a♭ . 1) (:ȧ♭ . 1)
                         (:a . 1) (:ȧ . 1) (:a♯ . 1) (:b♭ . 1) (:ḃ♭ . 1)
                         (:b♮ . 1) (:ḃ♮ . 1) (:b♯ . 1)
                         (:c . 1)

                         (:dʼ . 1) (:eʼ . 1) (:gʼ . 1) (:aʼ . 1) (:b♮ʼ . 1)
                         ))
       (note-scale (sort (copy-list note-selection)
                         #'<
                         :key (lambda (id)
                                (+ (* (cdr id) 1200)
                                   (vicentino-tunings:interval-size :tuning1 :c :up (car id))))))
       (tick-positions (mapcar (lambda (id)
                                       (* upscale 0.1
                                          (+ (* (cdr id) 1200)
                                             (vicentino-tunings:interval-size :tuning1 :c :up (car id)))))
                                     note-scale))
       (tick-labels (mapcar (lambda (id) (format nil "~a" (car id))) note-scale))
       (label-padding (* upscale 2))
       (origin (pt 0 0))
       (tick (circ 0 0 (* upscale 0.2) :style-update '(:fill :fill)))
       (ticks-gr (gr (mapcar (lambda (x) (cp tick origin (pt x 0))) tick-positions)))
       (tick-labels-gr (gr (mapcar (lambda (x text) (make-text text (pt x (- label-padding))))
                                   tick-positions
                                   tick-labels)))
       (size-list nil)
       (crossings-gr nil)
       (arcs-gr (gr (do ((remainder tick-positions (rest remainder))
                         (result nil))
                        ((null remainder) result)
                      (loop for el in (rest remainder)
                            do (let* ((a (pt (first remainder) 0))
                                      (b (pt el 0)))
                                 (push (distance-between-points a b) size-list)
                                 (push (circ (+ (value (x a))
                                                (* 1/2 (distance-between-points a b)))
                                             (* 1/2 (distance-between-points a b))
                                             (* upscale .09))
                                       crossings-gr)
                                 (push (arc a (* 1/2 (distance-between-points a b)) 180 0
                                            :mode :point)
                                       result))))))
       (tangent-start (* upscale -2))
       (tangent-end (+ (car (last tick-positions)) (* upscale 2)))
       (tangents-gr (gr (mapcar (lambda (y) (ln (pt tangent-start (* 1/2 y))
                                                (pt tangent-end (* 1/2 y))
                                                :style-update '(:line-type :dotted)))
                                (remove-duplicates size-list))))
       (btikz (make-backend-tikz :filename "vicentino-map-31-sesto.tex")))
  (draw-with-multiple-backends (list btikz) (list ticks-gr tick-labels-gr
                                                  arcs-gr
                                                  tangents-gr
                                                  (gr crossings-gr)))
  (compile-tikz btikz))




;; 1/4-comma, 31 per octave, plus sesto ordine, plus hypothetical notes
(let* ((upscale 4)
       (note-selection '((:c . -1) (:ċ . 0) (:c♯ . 0) (:d♭ . 0) (:ḋ♭ . 0)
                         (:d . 0) (:ḋ . 0) (:d♯ . 0) (:e♭ . 0) (:ė♭ . 0)
                         (:e . 0) (:ė . 0) (:e♯ . 0)
                         (:f . 0) (:ḟ . 0) (:f♯ . 0) (:g♭ . 0) (:ġ♭ . 0)
                         (:g . 0) (:ġ . 0) (:g♯ . 0) (:a♭ . 0) (:ȧ♭ . 0)
                         (:a . 0) (:ȧ . 0) (:a♯ . 0) (:b♭ . 0) (:ḃ♭ . 0)
                         (:b♮ . 0) (:ḃ♮ . 0) (:b♯ . 0)

                         (:dʼ . 0) (:eʼ . 0) (:gʼ . 0) (:aʼ . 0) (:b♮ʼ . 0)
                         (:cʼ . 0) (:fʼ . 0) (:b♭ʼ . 0) (:c-ʼ . -1)

                         (:c . 0) (:ċ . 1) (:c♯ . 1) (:d♭ . 1) (:ḋ♭ . 1)
                         (:d . 1) (:ḋ . 1) (:d♯ . 1) (:e♭ . 1) (:ė♭ . 1)
                         (:e . 1) (:ė . 1) (:e♯ . 1)
                         (:f . 1) (:ḟ . 1) (:f♯ . 1) (:g♭ . 1) (:ġ♭ . 1)
                         (:g . 1) (:ġ . 1) (:g♯ . 1) (:a♭ . 1) (:ȧ♭ . 1)
                         (:a . 1) (:ȧ . 1) (:a♯ . 1) (:b♭ . 1) (:ḃ♭ . 1)
                         (:b♮ . 1) (:ḃ♮ . 1) (:b♯ . 1)
                         (:c . 1)

                         (:dʼ . 1) (:eʼ . 1) (:gʼ . 1) (:aʼ . 1) (:b♮ʼ . 1)
                         (:cʼ . 1) (:fʼ . 1) (:b♭ʼ . 1) (:c-ʼ . 0)
                         ))
       (note-scale (sort (copy-list note-selection)
                         #'<
                         :key (lambda (id)
                                (+ (* (cdr id) 1200)
                                   (vicentino-tunings:interval-size :tuning1
                                                                    :c
                                                                    :up
                                                                    (car id))))))
       (tick-positions (mapcar (lambda (id)
                                       (* upscale 0.1
                                          (+ (* (cdr id) 1200)
                                             (vicentino-tunings:interval-size :tuning1 :c :up (car id)))))
                                     note-scale))
       (tick-labels (mapcar (lambda (id) (format nil "~a" (car id))) note-scale))
       (label-padding (* upscale 2))
       (origin (pt 0 0))
       (tick (circ 0 0 (* upscale 0.2) :style-update '(:fill :fill)))
       (ticks-gr (gr (mapcar (lambda (x) (cp tick origin (pt x 0))) tick-positions)))
       (tick-labels-gr (gr (mapcar (lambda (x text) (make-text text (pt x (- label-padding))))
                                   tick-positions
                                   tick-labels)))
       (size-list nil)
       (crossings-gr nil)
       (arcs-gr (gr (do ((remainder tick-positions (rest remainder))
                         (result nil))
                        ((null remainder) result)
                      (loop for el in (rest remainder)
                            do (let* ((a (pt (first remainder) 0))
                                      (b (pt el 0)))
                                 (push (distance-between-points a b) size-list)
                                 (push (circ (+ (value (x a))
                                                (* 1/2 (distance-between-points a b)))
                                             (* 1/2 (distance-between-points a b))
                                             (* upscale .09))
                                       crossings-gr)
                                 (push (arc a (* 1/2 (distance-between-points a b)) 180 0
                                            :mode :point)
                                       result))))))
       (tangent-start (* upscale -2))
       (tangent-end (+ (car (last tick-positions)) (* upscale 2)))
       (tangents-gr (gr (mapcar (lambda (y) (ln (pt tangent-start (* 1/2 y))
                                                (pt tangent-end (* 1/2 y))
                                                :style-update '(:line-type :dotted)))
                                (remove-duplicates size-list))))
       (btikz (make-backend-tikz :filename "vicentino-map-31-sesto-plus.tex")))
  (draw-with-multiple-backends (list btikz) (list ticks-gr tick-labels-gr
                                                  arcs-gr
                                                  tangents-gr
                                                  (gr crossings-gr)))
  (compile-tikz btikz))




;; 1/4-comma, 31, diesis and major thirds only
(let* ((upscale 4)
       (note-selection '((:a . 0) (:ȧ . 0) (:a♯ . 0)))
       (note-scale (sort (copy-list note-selection)
                         #'<
                         :key (lambda (id)
                                (+ (* (cdr id) 1200)
                                   (vicentino-tunings:interval-size :tuning1 :c :up (car id))))))
       (tick-positions (mapcar (lambda (id)
                                       (* upscale 0.1
                                          (+ (* (cdr id) 1200)
                                             (vicentino-tunings:interval-size :tuning1 :c :up (car id)))))
                                     note-scale))
       (tick-labels (mapcar (lambda (id) (format nil "~a" (car id))) note-scale))
       (label-padding (* upscale 0.7))
       (origin (pt 0 0))
       (tick (circ 0 0 (* upscale 0.2) :style-update '(:fill :fill)))
       (ticks-gr (gr (mapcar (lambda (x) (cp tick origin (pt x 0))) tick-positions)))
       (tick-labels-gr (gr (mapcar (lambda (x text) (make-text text (pt x (- label-padding))))
                                   tick-positions
                                   tick-labels)))
       (size-list nil)
       (crossings-gr nil)
       (arcs-gr (gr (do ((remainder tick-positions (rest remainder))
                         (result nil))
                        ((null remainder) (progn (setf size-list (remove-similar size-list 0.8))
                                                 result))
                      (loop for el in (rest remainder)
                            do (let* ((a (pt (first remainder) 0))
                                      (b (pt el 0)))
                                 (push (distance-between-points a b) size-list)
                                 (push (circ (+ (value (x a))
                                                (* 1/2 (distance-between-points a b)))
                                             (* 1/2 (distance-between-points a b))
                                             (* upscale .09))
                                       crossings-gr)
                                 (push (arc a (* 1/2 (distance-between-points a b)) 180 0
                                            :mode :point)
                                       result))))))
       (tangent-start (+ (* upscale -2) (first tick-positions)))
       (tangent-end (+ (car (last tick-positions)) (* upscale 2)))
       (tangents-gr (gr (mapcar (lambda (y) (ln (pt tangent-start (* 1/2 y))
                                                (pt tangent-end (* 1/2 y))
                                                :style-update '(:line-type :dotted)))
                                (remove-duplicates size-list))))
       (interval-label-x (+ tangent-end 1))
       (interval-labels '())
       (interval-labels-gr (gr (mapcar (lambda (yy text)
                                         (make-text text
                                                    (pt interval-label-x (* 1/2 yy))
                                                    :h-align :left))
                                       (sort (copy-list (remove-similar size-list))
                                             #'<)
                                       interval-labels)))
       (btikz (make-backend-tikz :filename "vicentino-map-diesis.tex")))
  (draw-with-multiple-backends (list btikz) (list ticks-gr tick-labels-gr
                                                  arcs-gr
                                                  tangents-gr
                                                  (gr crossings-gr)
                                                  interval-labels-gr))
  (compile-tikz btikz))


(let* ((upscale 1)
       (note-selection '((:ḟ . 0) (:ȧ . 0) (:d♭ . 0)))
       (note-scale (sort (copy-list note-selection)
                         #'<
                         :key (lambda (id)
                                (+ (* (cdr id) 1200)
                                   (vicentino-tunings:interval-size :tuning1 :c :up (car id))))))
       (tick-positions (mapcar (lambda (id)
                                       (* upscale 0.1
                                          (+ (* (cdr id) 1200)
                                             (vicentino-tunings:interval-size :tuning1 :c :up (car id)))))
                                     note-scale))
       (tick-labels (mapcar (lambda (id) (format nil "~a" (car id))) note-scale))
       (label-padding 2)
       (origin (pt 0 0))
       (tick (circ 0 0 (* upscale 0.2) :style-update '(:fill :fill)))
       (ticks-gr (gr (mapcar (lambda (x) (cp tick origin (pt x 0))) tick-positions)))
       (tick-labels-gr (gr (mapcar (lambda (x text) (make-text text (pt x (- label-padding))))
                                   tick-positions
                                   tick-labels)))
       (size-list nil)
       (crossings-gr nil)
       (arcs-gr (gr (do ((remainder tick-positions (rest remainder))
                         (result nil))
                        ((null remainder) (progn (setf size-list (remove-similar size-list 0.8))
                                                 result))
                      (loop for el in (rest remainder)
                            do (let* ((a (pt (first remainder) 0))
                                      (b (pt el 0)))
                                 (push (distance-between-points a b) size-list)
                                 (push (circ (+ (value (x a))
                                                (* 1/2 (distance-between-points a b)))
                                             (* 1/2 (distance-between-points a b))
                                             (* upscale .09))
                                       crossings-gr)
                                 (push (arc a (* 1/2 (distance-between-points a b)) 180 0
                                            :mode :point)
                                       result))))))
       (tangent-start (+ (* upscale -2) (first tick-positions)))
       (tangent-end (+ (car (last tick-positions)) (* upscale 2)))
       (tangents-gr (gr (mapcar (lambda (y) (ln (pt tangent-start (* 1/2 y))
                                                (pt tangent-end (* 1/2 y))
                                                :style-update '(:line-type :dotted)))
                                (remove-duplicates size-list))))
       (interval-label-x (+ tangent-end 1))
       (interval-labels '())
       (interval-labels-gr (gr (mapcar (lambda (yy text)
                                         (make-text text
                                                    (pt interval-label-x (* 1/2 yy))
                                                    :h-align :left))
                                       (sort (copy-list (remove-similar size-list))
                                             #'<)
                                       interval-labels)))
       (btikz (make-backend-tikz :filename "vicentino-map-terza-maggiore.tex")))
  (draw-with-multiple-backends (list btikz) (list ticks-gr tick-labels-gr
                                                  arcs-gr
                                                  tangents-gr
                                                  (gr crossings-gr)
                                                  interval-labels-gr))
  (compile-tikz btikz))
