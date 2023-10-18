(in-package :drawer)

;; Studio31-Publikation, Artikel «Das apaptiv-reine System»

(let* ((bsvg (make-backend-svg :width 3100 :height 900 :filename "vicentino-num-c-c.svg"))
       (btikz (make-backend-tikz :filename "vicentino-num-c-c.tex"))
       (white-width 10)
       (white-width-2 (* 1/2 white-width))
       (white-front 12)
       (white-front-2 (* 1/2 white-front))
       (black-width 7)
       (black-width-2 (* 1/2 black-width))
       (black-a-length 9)
       (black-b-length 12)
       (tastino-width 5)
       (tastino-width-2 (* 1/2 tastino-width))
       (tastino-length 12)
       (origin (pt 0 0))
       (anchor (pt 12 12))
       (air .3)
       (white-shift-pt (pt white-width-2 white-front-2))
       (lowest-shape (ln-shape origin (list white-width
                                            white-front
                                            (- black-width-2)
                                            (+ black-a-length black-b-length)
                                            (- (- white-width black-width-2)))
                               :shift air :shift-pt white-shift-pt))
       (highest-shape (ln-shape origin (list white-width
                                             (+ white-front (+ black-a-length black-b-length))
                                             (- (- white-width black-width-2))
                                             (- (+ black-a-length black-b-length))
                                             (- black-width-2))
                                :shift air :shift-pt white-shift-pt))
       (white-blind (ln-shape origin (list white-width
                                           (+ white-front (+ black-a-length black-b-length))
                                           (- white-width))
                              :shift air :shift-pt white-shift-pt))
       (mid-shape (ln-shape origin (list white-width
                                         white-front
                                         (- black-width-2)
                                         (+ black-a-length black-b-length)
                                         (- (- white-width black-width))
                                         (- (+ black-a-length black-b-length))
                                         (- black-width-2))
                            :shift air :shift-pt white-shift-pt))
       (left-tastino-shape (ln-shape origin (list white-width
                                                  (- (+ white-front (+ black-a-length black-b-length))
                                                     tastino-length)
                                                  (- tastino-width-2)
                                                  tastino-length
                                                  (- (- white-width black-width-2 tastino-width-2))
                                                  (- (+ black-a-length black-b-length))
                                                  (- black-width-2))
                                     :shift air :shift-pt white-shift-pt))
       (right-tastino-shape (ln-shape origin (list white-width
                                                   white-front
                                                   (- black-width-2)
                                                   (+ black-a-length black-b-length)
                                                   (- (- white-width black-width-2 tastino-width-2))
                                                   (- tastino-length)
                                                   (- tastino-width-2))
                                      :shift air :shift-pt white-shift-pt))
       (black-a (ln-shape origin (list black-width
                                       black-a-length
                                       (- black-width))
                          :shift air :shift-pt (pt black-width-2 (* 1/2 black-a-length))))
       (black-b (ln-shape origin (list black-width
                                       black-b-length
                                       (- black-width))
                          :shift air :shift-pt (pt black-width-2 (* 1/2 black-b-length))))
       (tastino (ln-shape origin (list tastino-width
                                       tastino-length
                                       (- tastino-width))
                          :shift air :shift-pt (pt tastino-width-2 (* 1/2 tastino-length))))
       (first-tastino (cp tastino origin
                          (add anchor (pt (- (* 3 white-width) tastino-width-2)
                                          (- (+ white-front (+ black-a-length black-b-length))
                                             tastino-length)))))
       (black-pair (gr (list (cp black-a origin (pt (- white-width black-width-2)
                                                    white-front))
                             (cp black-b origin (pt (- white-width
                                                       black-width-2)
                                                    (+ white-front black-a-length))))))
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
       (lbl-name-primo-stembridge (make-text-array '("C" "D" "E" "F" "G" "A" "B")
                                                   white-shift-pt (pt white-width 0)))
       (lbl-num-primo-stembridge (make-text-array '("1" "4" "7" "9" "12" "15" "18")
                                                  white-shift-pt (pt white-width 0)))
       (lbl-name-secondo-stembridge (make-text-array '("C♯" "E♭" nil "F♯" "G♯" "B♭")
                                                     (pt white-width (+ white-front
                                                                        (* 1/2 black-a-length)))
                                                     (pt white-width 0)))
       (lbl-num-secondo-stembridge (make-text-array '("2" "6" nil "10" "13" "17")
                                                    (pt white-width (+ white-front
                                                                       (* 1/2 black-a-length)))
                                                    (pt white-width 0)))
       (lbl-name-terzo-stembridge (make-text-array '("D♭" "D♯" "E♯" "G♭" "A♭" "A♯" "B♯")
                                                   (pt white-width (+ white-front
                                                                      (+ black-a-length
                                                                         (* 1/2 black-b-length))))
                                                   (pt white-width 0)))
       (lbl-num-terzo-stembridge (make-text-array '("3" "5" "8" "11" "14" "16" "19")
                                                  (pt white-width (+ white-front
                                                                     (+ black-a-length
                                                                        (* 1/2 black-b-length))))
                                                  (pt white-width 0)))
       (text-air 1.4)
       (stembridge (gr (list (cp right-tastino-shape origin anchor)
                             octave-19-body
                             (cp left-tastino-shape origin (right-of anchor (* 6 white-width)))
                             (cp first-tastino anchor (right-of anchor (* 4 white-width)))
                             (cp lbl-name-primo-stembridge origin (below anchor text-air))
                             (cp lbl-num-primo-stembridge origin (above anchor text-air))
                             (cp lbl-name-secondo-stembridge origin (below anchor text-air))
                             (cp lbl-num-secondo-stembridge origin (above anchor text-air))
                             (cp lbl-name-terzo-stembridge origin (below anchor text-air))
                             (cp lbl-num-terzo-stembridge origin (above anchor text-air))
                             )))
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
                                         (pt white-width (+ white-front (* 1/2 black-a-length)))
                                         (pt white-width 0)))
       (lbl-black-back (make-text-array '("D♭" "D♯" nil "G♭" "A♭" "A♯" nil "d♭" "d♯" nil "g♭" "a♭" "a♯" nil "d♭'" "d♯'" nil "g♭'" "a♭'" "a♯'" nil "d♭''" "d♯''" nil "g♭''" "a♭''" "a♯''")
                                        (pt white-width (+ white-front (+ black-a-length
                                                                          (* 1/2 black-b-length))))
                                        (pt white-width 0)))
       (lbl-tastini (make-text-array '(nil nil "E♯" nil nil nil "B♯" nil nil "e♯" nil nil nil "b♯" nil nil "e♯'" nil nil nil "b♯'" nil nil "e♯''")
                                     (pt white-width (- (+ white-front (+ black-a-length black-b-length))
                                                        (* 1/2 tastino-length)))
                                     (pt white-width 0)))
       (keyboard-labels (gr (list (cp lbl-white origin anchor)
                                  (cp lbl-black-front origin anchor)
                                  (cp lbl-black-back origin anchor)
                                  (cp lbl-tastini origin anchor)))))
  (draw-with-multiple-backends (list bsvg
                                     bhtml
                                     btikz)
                               (list stembridge

                                     ))
  (compile-tikz btikz))



;; Studio31-Publikation, Artikel von Christopher Stembridge

(let* ((btikz (make-backend-tikz :filename "stembridge-kbd-draft1.tex"))
       (white-width 10)
       (white-width-2 (* 1/2 white-width))
       (white-front 12)
       (white-front-2 (* 1/2 white-front))
       (black-width 7)
       (black-width-2 (* 1/2 black-width))
       (black-a-length 9)
       (black-b-length 12)
       (tastino-width 5)
       (tastino-width-2 (* 1/2 tastino-width))
       (tastino-length 12)
       (origin (pt 0 0))
       (anchor (pt 12 12))
       (air .3)
       (white-shift-pt (pt white-width-2 white-front-2))
       (mid-shape (ln-shape origin (list white-width
                                         white-front
                                         (- black-width-2)
                                         (+ black-a-length black-b-length)
                                         (- (- white-width black-width))
                                         (- (+ black-a-length black-b-length))
                                         (- black-width-2))
                            :shift air :shift-pt white-shift-pt))
       (left-tastino-shape (ln-shape origin (list white-width
                                                  (- (+ white-front (+ black-a-length black-b-length))
                                                     tastino-length)
                                                  (- tastino-width-2)
                                                  tastino-length
                                                  (- (- white-width black-width-2 tastino-width-2))
                                                  (- (+ black-a-length black-b-length))
                                                  (- black-width-2))
                                     :shift air :shift-pt white-shift-pt))
       (right-tastino-shape (ln-shape origin (list white-width
                                                   white-front
                                                   (- black-width-2)
                                                   (+ black-a-length black-b-length)
                                                   (- (- white-width black-width-2 tastino-width-2))
                                                   (- tastino-length)
                                                   (- tastino-width-2))
                                      :shift air :shift-pt white-shift-pt))
       (black-a (ln-shape origin (list black-width
                                       black-a-length
                                       (- black-width))
                          :shift air :shift-pt (pt black-width-2 (* 1/2 black-a-length))))
       (black-b (ln-shape origin (list black-width
                                       black-b-length
                                       (- black-width))
                          :shift air :shift-pt (pt black-width-2 (* 1/2 black-b-length))))
       (tastino (ln-shape origin (list tastino-width
                                       tastino-length
                                       (- tastino-width))
                          :shift air :shift-pt (pt tastino-width-2 (* 1/2 tastino-length))))
       (first-tastino (cp tastino origin
                          (add anchor (pt (- (* 3 white-width) tastino-width-2)
                                          (- (+ white-front (+ black-a-length black-b-length))
                                             tastino-length)))))
       (black-pair (gr (list (cp black-a origin (pt (- white-width black-width-2)
                                                    white-front))
                             (cp black-b origin (pt (- white-width
                                                       black-width-2)
                                                    (+ white-front black-a-length))))))
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
       (lbl-name-primo-stembridge (make-text-array '("C" "D" "E" "F" "G" "A" "B")
                                                   white-shift-pt (pt white-width 0)))
       (lbl-num-primo-stembridge (make-text-array '("1" "4" "7" "9" "12" "15" "18")
                                                  white-shift-pt (pt white-width 0)))
       (lbl-name-secondo-stembridge (make-text-array '("C♯" "E♭" nil "F♯" "G♯" "B♭")
                                                     (pt white-width (+ white-front
                                                                        (* 1/2 black-a-length)))
                                                     (pt white-width 0)))
       (lbl-num-secondo-stembridge (make-text-array '("2" "6" nil "10" "13" "17")
                                                    (pt white-width (+ white-front
                                                                       (* 1/2 black-a-length)))
                                                    (pt white-width 0)))
       (lbl-name-terzo-stembridge (make-text-array '("D♭" "D♯" "E♯" "G♭" "A♭" "A♯" "B♯")
                                                   (pt white-width (+ white-front
                                                                      (+ black-a-length
                                                                         (* 1/2 black-b-length))))
                                                   (pt white-width 0)))
       (lbl-num-terzo-stembridge (make-text-array '("3" "5" "8" "11" "14" "16" "19")
                                                  (pt white-width (+ white-front
                                                                     (+ black-a-length
                                                                        (* 1/2 black-b-length))))
                                                  (pt white-width 0)))
       (text-air 1.4)
       (stembridge (gr (list (cp right-tastino-shape origin anchor)
                             octave-19-body
                             (cp left-tastino-shape origin (right-of anchor (* 6 white-width)))
                             (cp first-tastino anchor (right-of anchor (* 4 white-width)))
                             (cp lbl-name-primo-stembridge origin (below anchor text-air))
                             (cp lbl-num-primo-stembridge origin (above anchor text-air))
                             (cp lbl-name-secondo-stembridge origin (below anchor text-air))
                             (cp lbl-num-secondo-stembridge origin (above anchor text-air))
                             (cp lbl-name-terzo-stembridge origin (below anchor text-air))
                             (cp lbl-num-terzo-stembridge origin (above anchor text-air))))))
  (draw-with-multiple-backends (list btikz)
                               (list stembridge))
  (compile-tikz btikz))

;; playground
(let* ((bsvg (make-backend-svg 3100 900))
       (bhtml (make-backend-html))
       (btikz (make-backend-tikz))
       (white-width 10)
       (white-width-2 (* 1/2 white-width))
       (white-front 12)
       (white-front-2 (* 1/2 white-front))
       (black-width 7)
       (black-width-2 (* 1/2 black-width))
       (black-a-length 9)
       (black-b-length 12)
       (tastino-width 5)
       (tastino-width-2 (* 1/2 tastino-width))
       (tastino-length 12)
       (origin (pt 0 0))
       (anchor (pt 12 12))
       (air .3)
       (white-shift-pt (pt white-width-2 white-front-2))
       (lowest-shape (ln-shape origin (list white-width
                                            white-front
                                            (- black-width-2)
                                            (+ black-a-length black-b-length)
                                            (- (- white-width black-width-2)))
                               :shift air :shift-pt white-shift-pt))
       (highest-shape (ln-shape origin (list white-width
                                             (+ white-front (+ black-a-length black-b-length))
                                             (- (- white-width black-width-2))
                                             (- (+ black-a-length black-b-length))
                                             (- black-width-2))
                                :shift air :shift-pt white-shift-pt))
       (white-blind (ln-shape origin (list white-width
                                           (+ white-front (+ black-a-length black-b-length))
                                           (- white-width))
                              :shift air :shift-pt white-shift-pt))
       (mid-shape (ln-shape origin (list white-width
                                         white-front
                                         (- black-width-2)
                                         (+ black-a-length black-b-length)
                                         (- (- white-width black-width))
                                         (- (+ black-a-length black-b-length))
                                         (- black-width-2))
                            :shift air :shift-pt white-shift-pt))
       (left-tastino-shape (ln-shape origin (list white-width
                                                  (- (+ white-front (+ black-a-length black-b-length))
                                                     tastino-length)
                                                  (- tastino-width-2)
                                                  tastino-length
                                                  (- (- white-width black-width-2 tastino-width-2))
                                                  (- (+ black-a-length black-b-length))
                                                  (- black-width-2))
                                     :shift air :shift-pt white-shift-pt))
       (right-tastino-shape (ln-shape origin (list white-width
                                                   white-front
                                                   (- black-width-2)
                                                   (+ black-a-length black-b-length)
                                                   (- (- white-width black-width-2 tastino-width-2))
                                                   (- tastino-length)
                                                   (- tastino-width-2))
                                      :shift air :shift-pt white-shift-pt))
       (black-a (ln-shape origin (list black-width
                                       black-a-length
                                       (- black-width))
                          :shift air :shift-pt (pt black-width-2 (* 1/2 black-a-length))))
       (black-b (ln-shape origin (list black-width
                                       black-b-length
                                       (- black-width))
                          :shift air :shift-pt (pt black-width-2 (* 1/2 black-b-length))))
       (tastino (ln-shape origin (list tastino-width
                                       tastino-length
                                       (- tastino-width))
                          :shift air :shift-pt (pt tastino-width-2 (* 1/2 tastino-length))))
       (first-tastino (cp tastino origin
                          (add anchor (pt (- (* 3 white-width) tastino-width-2)
                                          (- (+ white-front (+ black-a-length black-b-length))
                                             tastino-length)))))
       (black-pair (gr (list (cp black-a origin (pt (- white-width black-width-2)
                                                    white-front))
                             (cp black-b origin (pt (- white-width
                                                       black-width-2)
                                                    (+ white-front black-a-length))))))
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
       (lbl-name-primo-stembridge (make-text-array '("C" "D" "E" "F" "G" "A" "B")
                                                   white-shift-pt (pt white-width 0)))
       (lbl-num-primo-stembridge (make-text-array '("1" "4" "7" "9" "12" "15" "18")
                                                  white-shift-pt (pt white-width 0)))
       (lbl-name-secondo-stembridge (make-text-array '("C♯" "E♭" nil "F♯" "G♯" "B♭")
                                                     (pt white-width (+ white-front
                                                                        (* 1/2 black-a-length)))
                                                     (pt white-width 0)))
       (lbl-num-secondo-stembridge (make-text-array '("2" "6" nil "10" "13" "17")
                                                    (pt white-width (+ white-front
                                                                       (* 1/2 black-a-length)))
                                                    (pt white-width 0)))
       (lbl-name-terzo-stembridge (make-text-array '("D♭" "D♯" "E♯" "G♭" "A♭" "A♯" "B♯")
                                                   (pt white-width (+ white-front
                                                                      (+ black-a-length
                                                                         (* 1/2 black-b-length))))
                                                   (pt white-width 0)))
       (lbl-num-terzo-stembridge (make-text-array '("3" "5" "8" "11" "14" "16" "19")
                                                  (pt white-width (+ white-front
                                                                     (+ black-a-length
                                                                        (* 1/2 black-b-length))))
                                                  (pt white-width 0)))
       (text-air 1.4)
       (stembridge (gr (list (cp right-tastino-shape origin anchor)
                             octave-19-body
                             (cp left-tastino-shape origin (right-of anchor (* 6 white-width)))
                             (cp first-tastino anchor (right-of anchor (* 4 white-width)))
                             (cp lbl-name-primo-stembridge origin (below anchor text-air))
                             (cp lbl-num-primo-stembridge origin (above anchor text-air))
                             (cp lbl-name-secondo-stembridge origin (below anchor text-air))
                             (cp lbl-num-secondo-stembridge origin (above anchor text-air))
                             (cp lbl-name-terzo-stembridge origin (below anchor text-air))
                             (cp lbl-num-terzo-stembridge origin (above anchor text-air))
                             )))
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
                                         (pt white-width (+ white-front (* 1/2 black-a-length)))
                                         (pt white-width 0)))
       (lbl-black-back (make-text-array '("D♭" "D♯" nil "G♭" "A♭" "A♯" nil "d♭" "d♯" nil "g♭" "a♭" "a♯" nil "d♭'" "d♯'" nil "g♭'" "a♭'" "a♯'" nil "d♭''" "d♯''" nil "g♭''" "a♭''" "a♯''")
                                        (pt white-width (+ white-front (+ black-a-length
                                                                          (* 1/2 black-b-length))))
                                        (pt white-width 0)))
       (lbl-tastini (make-text-array '(nil nil "E♯" nil nil nil "B♯" nil nil "e♯" nil nil nil "b♯" nil nil "e♯'" nil nil nil "b♯'" nil nil "e♯''")
                                     (pt white-width (- (+ white-front (+ black-a-length black-b-length))
                                                        (* 1/2 tastino-length)))
                                     (pt white-width 0)))
       (keyboard-labels (gr (list (cp lbl-white origin anchor)
                                  (cp lbl-black-front origin anchor)
                                  (cp lbl-black-back origin anchor)
                                  (cp lbl-tastini origin anchor)))))
  (draw-with-multiple-backends (list bsvg
                                     bhtml
                                     btikz)
                               (list stembridge

                                     ))
  (compile-tikz btikz))
