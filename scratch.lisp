(in-package :drawer)

(ql:quickload :vicentino-tunings)

(format t "~&~a" (vicentino-tunings:interval-size :tuning1 :c :up :c))

;; 1/4 comma meantone, diatonic scale
(let* ((upscale 2)
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
       (btikz (make-backend-tikz :filename "vicentino-map-diatonic.tex")))
  (draw-with-multiple-backends (list btikz) (list ticks-gr tick-labels-gr
                                                  arcs-gr
                                                  tangents-gr
                                                  (gr crossings-gr)))
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
       (btikz (make-backend-tikz :filename "vicentino-map-19.tex")))
  (draw-with-multiple-backends (list btikz) (list ticks-gr tick-labels-gr
                                                  arcs-gr
                                                  tangents-gr
                                                  (gr crossings-gr)))
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




;; 1/4-comma, 31 per octave, fifths above every note (ultra-organ)
(let* ((upscale 5)
       (note-selection '((:c . -1) (:ċ . 0) (:c♯ . 0) (:d♭ . 0) (:ḋ♭ . 0)
                         (:d . 0) (:ḋ . 0) (:d♯ . 0) (:e♭ . 0) (:ė♭ . 0)
                         (:e . 0) (:ė . 0) (:e♯ . 0)
                         (:f . 0) (:ḟ . 0) (:f♯ . 0) (:g♭ . 0) (:ġ♭ . 0)
                         (:g . 0) (:ġ . 0) (:g♯ . 0) (:a♭ . 0) (:ȧ♭ . 0)
                         (:a . 0) (:ȧ . 0) (:a♯ . 0) (:b♭ . 0) (:ḃ♭ . 0)
                         (:b♮ . 0) (:ḃ♮ . 0) (:b♯ . 0)

                         (:cʼ . 0) (:ċʼ . 0) (:c♯ʼ . 0) (:d♭ʼ . 0) (:ḋ♭ʼ . 0)
                         (:dʼ . 0) (:ḋʼ . 0) (:d♯ʼ . 0) (:e♭ʼ . 0) (:ė♭ʼ . 0)
                         (:eʼ . 0) (:ėʼ . 0) (:e♯ʼ . 0)
                         (:fʼ . 0) (:ḟʼ . 0) (:f♯ʼ . 0) (:g♭ʼ . 0) (:ġ♭ʼ . 0)
                         (:gʼ . 0) (:ġʼ . 0) (:g♯ʼ . 0) (:a♭ʼ . 0) (:ȧ♭ʼ . 0)
                         (:aʼ . 0) (:ȧʼ . 0) (:a♯ʼ . 0) (:b♭ʼ . 0) (:ḃ♭ʼ . 0)
                         (:b♮ʼ . 0) (:ḃ♮ʼ . 0) (:b♯ʼ . 0)

                         (:c . 0) (:ċ . 1) (:c♯ . 1) (:d♭ . 1) (:ḋ♭ . 1)
                         (:d . 1) (:ḋ . 1) (:d♯ . 1) (:e♭ . 1) (:ė♭ . 1)
                         (:e . 1) (:ė . 1) (:e♯ . 1)
                         (:f . 1) (:ḟ . 1) (:f♯ . 1) (:g♭ . 1) (:ġ♭ . 1)
                         (:g . 1) (:ġ . 1) (:g♯ . 1) (:a♭ . 1) (:ȧ♭ . 1)
                         (:a . 1) (:ȧ . 1) (:a♯ . 1) (:b♭ . 1) (:ḃ♭ . 1)
                         (:b♮ . 1) (:ḃ♮ . 1) (:b♯ . 1)
                         (:c . 1)

                         (:cʼ . 1) (:ċʼ . 1) (:c♯ʼ . 1) (:d♭ʼ . 1) (:ḋ♭ʼ . 1)
                         (:dʼ . 1) (:ḋʼ . 1) (:d♯ʼ . 1) (:e♭ʼ . 1) (:ė♭ʼ . 1)
                         (:eʼ . 1) (:ėʼ . 1) (:e♯ʼ . 1)
                         (:fʼ . 1) (:ḟʼ . 1) (:f♯ʼ . 1) (:g♭ʼ . 1) (:ġ♭ʼ . 1)
                         (:gʼ . 1) (:ġʼ . 1) (:g♯ʼ . 1) (:a♭ʼ . 1) (:ȧ♭ʼ . 1)
                         (:aʼ . 1) (:ȧʼ . 1) (:a♯ʼ . 1) (:b♭ʼ . 1) (:ḃ♭ʼ . 1)
                         (:b♮ʼ . 1) (:ḃ♮ʼ . 1) (:b♯ʼ . 1)

                         (:c . 1)
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
       (label-padding (* upscale 1/2))
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
       (btikz (make-backend-tikz :filename "vicentino-map-31-ultra.tex")))
  (draw-with-multiple-backends (list btikz) (list ticks-gr tick-labels-gr
                                                  arcs-gr
                                                  tangents-gr
                                                  (gr crossings-gr)))
  (compile-tikz btikz))
