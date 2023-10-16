(in-package :drawer)

(let* ((bsvg (make-backend-svg 3100 900))
       (bhtml (make-backend-html))
       (btikz (make-backend-tikz))
       (white-width 10)
       (white-width-2 (* 1/2 white-width))
       (white-front 12)
       (white-front-2 (* 1/2 white-front))
       (black-width 7)
       (black-width-2 (* 1/2 black-width))
       (black-length 9)
       (tastino-width 5)
       (tastino-width-2 (* 1/2 tastino-width))
       (tastino-length 8)
       (origin (pt 0 0))
       (anchor (pt 12 12))
       (air .3)
       (white-shift-pt (pt white-width-2 white-front-2))
       (lowest-shape (ln-shape origin (list white-width
                                            white-front
                                            (- black-width-2)
                                            (* 2 black-length)
                                            (- (- white-width black-width-2)))
                               :shift air :shift-pt white-shift-pt))
       (highest-shape (ln-shape origin (list white-width
                                             (+ white-front (* 2 black-length))
                                             (- (- white-width black-width-2))
                                             (- (* 2 black-length))
                                             (- black-width-2))
                                :shift air :shift-pt white-shift-pt))
       (white-blind (ln-shape origin (list white-width
                                           (+ white-front (* 2 black-length))
                                           (- white-width))
                              :shift air :shift-pt white-shift-pt))
       (mid-shape (ln-shape origin (list white-width
                                         white-front
                                         (- black-width-2)
                                         (* 2 black-length)
                                         (- (- white-width black-width))
                                         (- (* 2 black-length))
                                         (- black-width-2))
                            :shift air :shift-pt white-shift-pt))
       (left-tastino-shape (ln-shape origin (list white-width
                                                  (- (+ white-front (* 2 black-length))
                                                     tastino-length)
                                                  (- tastino-width-2)
                                                  tastino-length
                                                  (- (- white-width black-width-2 tastino-width-2))
                                                  (- (* 2 black-length))
                                                  (- black-width-2))
                                     :shift air :shift-pt white-shift-pt))
       (right-tastino-shape (ln-shape origin (list white-width
                                                   white-front
                                                   (- black-width-2)
                                                   (* 2 black-length)
                                                   (- (- white-width black-width-2 tastino-width-2))
                                                   (- tastino-length)
                                                   (- tastino-width-2))
                                      :shift air :shift-pt white-shift-pt))
       (black (ln-shape origin (list black-width
                                     black-length
                                     (- black-width))
                        :shift air :shift-pt (pt black-width-2 (* 1/2 black-length))))
       (tastino (ln-shape origin (list tastino-width
                                       tastino-length
                                       (- tastino-width))
                          :shift air :shift-pt (pt tastino-width-2 (* 1/2 tastino-length))))
       (first-tastino (cp tastino origin
                          (add anchor (pt (- (* 3 white-width) tastino-width-2)
                                          (- (+ white-front (* 2 black-length))
                                             tastino-length)))))
       (black-pair (gr (list (cp black origin (pt (- white-width black-width-2)
                                                  white-front))
                             (cp black origin (pt (- white-width
                                                     black-width-2)
                                                  (+ white-front black-length))))))
       (octave-19-body (gr (list (cp black-pair origin anchor)
                                 (cp mid-shape origin (right-of anchor white-width))
                                 (cp black-pair origin (right-of anchor white-width))
                                 (cp left-tastino-shape origin (right-of anchor (* 2 white-width)))
                                 first-tastino
                                 (cp right-tastino-shape origin (right-of anchor (* 3 white-width)))
                                 (cp black-pair origin (right-of anchor (* 3 white-width)))
                                 (cp mid-shape origin (right-of anchor (* 4 white-width)))
                                 (cp black-pair origin (right-of anchor (* 4 white-width)))
                                 (cp mid-shape origin (right-of anchor (* 5 white-width)))
                                 (cp black-pair origin (right-of anchor (* 5 white-width)))
                                 )))
       (octave-19-joint (gr (list (cp left-tastino-shape origin (right-of anchor (* 6 white-width)))
                                  (cp first-tastino anchor (right-of anchor (* 4 white-width)))
                                  (cp right-tastino-shape origin (right-of anchor (* 7 white-width))))))
       (keyboard (gr (list (cp lowest-shape origin anchor)
                           octave-19-body
                           octave-19-joint
                           (cp octave-19-body anchor (right-of anchor (* 7 white-width)))
                           (cp octave-19-joint anchor (right-of anchor (* 7 white-width)))
                           (cp octave-19-body anchor (right-of anchor (* 14 white-width)))
                           (cp octave-19-joint anchor (right-of anchor (* 14 white-width)))
                           (cp octave-19-body anchor (right-of anchor (* 21 white-width)))
                           (cp highest-shape origin (right-of anchor (* 27 white-width)))
                           (cp white-blind origin (right-of anchor (* 28 white-width)))
                           )))
       (lbl-white (make-text-array '("C" "D" "E" "F" "G" "A" "B♮" "c" "d" "e" "f" "g" "a" "b♮" "c'" "d'" "e'" "f'" "g'" "a'" "b♮'" "c''" "d''" "e''" "f''" "g''" "a''" "b♮''" "c'''")
                                   white-shift-pt (pt white-width 0)))
       (lbl-black-front (make-text-array '("C♯" "E♭" nil "F♯" "G♯" "B♭" nil "c♯" "e♭" nil "f♯" "g♯" "b♭" nil "c♯'" "e♭'" nil "f♯'" "g♯'" "b♭'" nil "c♯''" "e♭''" nil "f♯''" "g♯''" "b♭''")
                                         (pt white-width (+ white-front (* 1/2 black-length)))
                                         (pt white-width 0)))
       (lbl-black-back (make-text-array '("D♭" "D♯" nil "G♭" "A♭" "A♯" nil "d♭" "d♯" nil "g♭" "a♭" "a♯" nil "d♭'" "d♯'" nil "g♭'" "a♭'" "a♯'" nil "d♭''" "d♯''" nil "g♭''" "a♭''" "a♯''")
                                         (pt white-width (+ white-front (* 3/2 black-length)))
                                         (pt white-width 0)))
       (lbl-tastini (make-text-array '(nil nil "E♯" nil nil nil "B♯" nil nil "e♯" nil nil nil "b♯" nil nil "e♯'" nil nil nil "b♯'" nil nil "e♯''")
                                     (pt white-width (- (+ white-front (* 2 black-length))
                                                        (* 1/2 tastino-length)))
                                         (pt white-width 0)))
       (keyboard-labels (gr (list (cp lbl-white origin anchor)
                                  (cp lbl-black-front origin anchor)
                                  (cp lbl-black-back origin anchor)
                                  (cp lbl-tastini origin anchor)))))
  (draw-with-multiple-backends (list bsvg
                                     bhtml
                                     btikz
                                     )
                               (list keyboard
                                     keyboard-labels
                                     (cp keyboard
                                         anchor
                                         (above anchor (+ white-front (* 2 black-length))))
                                     (cp keyboard-labels
                                         anchor
                                         (above anchor (+ white-front (* 2 black-length))))))
  (compile-tikz btikz))
