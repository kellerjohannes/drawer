(in-package :drawer)

;; For PhD, second tuning after Maniates 1996 (translation L'antica musica, Appendix VIII

(let* ((btikz (make-backend-tikz :filename "keyboard-second-tuning-maniates-1996.tex"))
       (white-width 10)
       (white-width-2 (* 1/2 white-width))
       (white-front 12)
       (white-front-2 (* 1/2 white-front))
       (black-width 7)
       (black-width-2 (* 1/2 black-width))
       (black-a-length 9)
       (black-b-length 9)
       (tastino-width 5)
       (tastino-width-2 (* 1/2 tastino-width))
       (tastino-length 9)
       (origin (pt 0 0))
       (anchor (pt 12 12))
       (air .3)
       (white-shift-pt (pt white-width-2 white-front-2))
       (manual-separation (+ white-front black-a-length black-b-length air))
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
       (octave-c-c-lower (gr (list
                              (cp right-tastino-shape origin anchor)
                              (cp black-pair origin anchor)
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
                              (cp left-tastino-shape origin (right-of anchor (* 6 white-width)))
                              (cp first-tastino anchor (right-of anchor (* 4 white-width)))
                              )))
       (octave-c-c-upper (gr (list
                              (cp lowest-shape origin anchor)
                              (cp black-pair origin anchor)
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
                              (cp left-tastino-shape origin (right-of anchor (* 6 white-width)))
                              (cp first-tastino anchor (right-of anchor (* 4 white-width)))
                              )))
       (keyboard (gr (list octave-c-c-lower
                           (cp octave-c-c-upper anchor (above anchor manual-separation)))))
       (alt-shift 1.2)
       (label-x-offset 0.3)
       (lbl-white-lower (make-text-array '("1C" "1D" "1E" "1F" "1G" "1A" "1B")
                                         (cp white-shift-pt origin (pt 0 alt-shift))
                                         (pt white-width 0)
                                         :x-offset label-x-offset))
       (lbl-white-lower-alt (make-text-array '("[C]" "[D]" "[E]" "[F]" "[G]" "[A]" "[B♮]")
                                             (cp white-shift-pt origin (pt 0 (- alt-shift)))
                                             (pt white-width 0)
                                             :x-offset label-x-offset))
       (lbl-black-front-lower (make-text-array '("2C♯" "2E♭" nil "2F♯" "2G♯" "2B♭")
                                               (pt white-width
                                                   (+ alt-shift
                                                      white-front
                                                      (* 1/2 black-a-length)))
                                               (pt white-width 0)
                                               :x-offset label-x-offset))
       (lbl-black-front-lower-alt (make-text-array '("[C♯]" "[E♭]" nil "[F♯]" "[G♯]" "[B♭]")
                                                   (pt white-width
                                                       (+ (- alt-shift)
                                                          white-front
                                                          (* 1/2 black-a-length)))
                                                   (pt white-width 0)
                                                   :x-offset label-x-offset))
       (lbl-black-back-lower (make-text-array '("3D♭" "3D♯" nil "3G♭" "3A♭" "3A♯")
                                              (pt white-width
                                                  (+ alt-shift
                                                     white-front
                                                     (+ black-a-length (* 1/2 black-b-length))))
                                              (pt white-width 0)
                                              :x-offset label-x-offset))
       (lbl-black-back-lower-alt (make-text-array '("[D♭]" "[D♯]" nil "[G♭]" "[A♭]" "[A♯]")
                                                  (pt white-width
                                                      (+ (- alt-shift)
                                                         white-front
                                                         (+ black-a-length (* 1/2 black-b-length))))
                                              (pt white-width 0)
                                              :x-offset label-x-offset))
       (lbl-tastini-lower (make-text-array '(nil nil "3E♯" nil nil nil "3B♯" )
                                           (pt white-width
                                               (- (+ alt-shift
                                                     white-front
                                                     (+ black-a-length black-b-length))
                                                  (* 1/2 tastino-length)))
                                           (pt white-width 0)
                                           :x-offset label-x-offset))
       (lbl-tastini-lower-alt (make-text-array '(nil nil "[E♯]" nil nil nil "[B♯]" )
                                               (pt white-width
                                                   (- (+ (- alt-shift)
                                                         white-front
                                                         (+ black-a-length black-b-length))
                                                      (* 1/2 tastino-length)))
                                               (pt white-width 0)
                                               :x-offset label-x-offset))
       (lbl-white-upper (make-text-array '("4Ċ" "4Ḋ" "4Ė" "4Ḟ" "4Ġ" "4Ȧ" "4Ḃ")
                                         (cp white-shift-pt origin (pt 0 alt-shift))
                                         (pt white-width 0)
                                         :x-offset label-x-offset))
       (lbl-white-upper-alt (make-text-array '("[C’]" "[D’]" "[E’]" "[F’]" "[G’]" "[A’]" "[B♮’]")
                                             (cp white-shift-pt origin (pt 0 (- alt-shift)))
                                             (pt white-width 0)
                                             :x-offset label-x-offset))
       (lbl-black-front-upper (make-text-array '("5Ḋ♭" "5Ė♭" nil "5Ġ♭" "5Ȧ♭" "5Ḃ♭")
                                               (pt white-width
                                                   (+ alt-shift
                                                      white-front (* 1/2 black-a-length)))
                                               (pt white-width 0)
                                               :x-offset label-x-offset))
       (lbl-black-front-upper-alt (make-text-array '("[C♯’]" "[D♯’]" nil "[F♯’]" "[G♯’]" "[B♭’]")
                                                   (pt white-width
                                                       (+ (- alt-shift)
                                                          white-front (* 1/2 black-a-length)))
                                                   (pt white-width 0)
                                                   :x-offset label-x-offset))
       (lbl-black-back-upper (make-text-array '("6Ɗ" "6Ẻ" nil "6Ɠ" "6Ả" "6Ɓ")
                                              (pt white-width
                                                  (+ alt-shift
                                                     white-front
                                                     (+ black-a-length (* 1/2 black-b-length))))
                                              (pt white-width 0)
                                              :x-offset label-x-offset))
       (lbl-black-back-upper-alt (make-text-array '("[D♭’]" "[E♭’]" nil "[F♯♯’]" "[A♭’]" "[A♯’]")
                                                  (pt white-width
                                                      (+ (- alt-shift)
                                                         white-front
                                                         (+ black-a-length (* 1/2 black-b-length))))
                                                  (pt white-width 0)
                                                  :x-offset label-x-offset))
       (lbl-tastini-upper (make-text-array '(nil nil "6Ƒ" nil nil nil "6Ƈ" )
                                           (pt white-width
                                               (- (+ alt-shift
                                                     white-front
                                                     (+ black-a-length black-b-length))
                                                  (* 1/2 tastino-length)))
                                           (pt white-width 0)
                                           :x-offset label-x-offset))
       (lbl-tastini-upper-alt (make-text-array '(nil nil "[E♯’]" nil nil nil "[B♯’]" )
                                               (pt white-width
                                                   (- (+ (- alt-shift)
                                                         white-front
                                                         (+ black-a-length black-b-length))
                                                      (* 1/2 tastino-length)))
                                               (pt white-width 0)
                                               :x-offset label-x-offset))
       (keyboard-labels (gr (list (cp lbl-white-lower origin anchor)
                                  (cp lbl-white-lower-alt origin anchor)
                                  (cp lbl-black-front-lower origin anchor)
                                  (cp lbl-black-front-lower-alt origin anchor)
                                  (cp lbl-black-back-lower origin anchor)
                                  (cp lbl-black-back-lower-alt origin anchor)
                                  (cp lbl-tastini-lower origin anchor)
                                  (cp lbl-tastini-lower-alt origin anchor)
                                  (cp lbl-white-upper origin (above anchor manual-separation))
                                  (cp lbl-white-upper-alt origin (above anchor manual-separation))
                                  (cp lbl-black-front-upper origin (above anchor manual-separation))
                                  (cp lbl-black-front-upper-alt origin (above anchor manual-separation))
                                  (cp lbl-black-back-upper origin (above anchor manual-separation))
                                  (cp lbl-black-back-upper-alt origin (above anchor manual-separation))
                                  (cp lbl-tastini-upper origin (above anchor manual-separation))
                                  (cp lbl-tastini-upper-alt origin (above anchor manual-separation))
                                  ))))
  (draw-with-multiple-backends (list btikz) (list keyboard keyboard-labels))
  (compile-tikz btikz))

;; For PhD, second tuning after Maniates 1975
(let* ((btikz (make-backend-tikz :filename "keyboard-second-tuning-maniates-1975.tex"))
       (white-width 10)
       (white-width-2 (* 1/2 white-width))
       (white-front 12)
       (white-front-2 (* 1/2 white-front))
       (black-width 7)
       (black-width-2 (* 1/2 black-width))
       (black-a-length 9)
       (black-b-length 9)
       (tastino-width 5)
       (tastino-width-2 (* 1/2 tastino-width))
       (tastino-length 9)
       (origin (pt 0 0))
       (anchor (pt 12 12))
       (air .3)
       (white-shift-pt (pt white-width-2 white-front-2))
       (manual-separation (+ white-front black-a-length black-b-length air))
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
       (octave-c-c-lower (gr (list
                              (cp right-tastino-shape origin anchor)
                              (cp black-pair origin anchor)
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
                              (cp left-tastino-shape origin (right-of anchor (* 6 white-width)))
                              (cp first-tastino anchor (right-of anchor (* 4 white-width)))
                              )))
       (octave-c-c-upper (gr (list
                              (cp lowest-shape origin anchor)
                              (cp black-pair origin anchor)
                              (cp mid-shape origin (right-of anchor white-width))
                              (cp black-pair origin (right-of anchor white-width))
                              (cp highest-shape origin (right-of anchor (* 2 white-width)))
                              (cp lowest-shape origin (right-of anchor (* 3 white-width)))
                              (cp black-pair origin (right-of anchor (* 3 white-width)))
                              (cp mid-shape origin (right-of anchor (* 4 white-width)))
                              (cp black-pair origin (right-of anchor (* 4 white-width)))
                              (cp mid-shape origin (right-of anchor (* 5 white-width)))
                              (cp black-pair origin (right-of anchor (* 5 white-width)))
                              (cp highest-shape origin (right-of anchor (* 6 white-width)))
                              )))
       (keyboard (gr (list octave-c-c-lower
                           (cp octave-c-c-upper anchor (above anchor manual-separation)))))
       (alt-shift 1.2)
       (lbl-white-lower (make-text-array '("1c" "1d" "1e" "1f" "1g" "1a" "1b")
                                         (cp white-shift-pt origin (pt 0 alt-shift))
                                         (pt white-width 0)))
       (lbl-white-lower-alt (make-text-array '("[C]" "[D]" "[E]" "[F]" "[G]" "[A]" "[B♮]")
                                             (cp white-shift-pt origin (pt 0 (- alt-shift)))
                                             (pt white-width 0)))
       (lbl-black-front-lower (make-text-array '("2c♯" "2e♭" nil "2f♯" "2g♯" "2b♭")
                                               (pt white-width
                                                   (+ alt-shift
                                                      white-front
                                                      (* 1/2 black-a-length)))
                                               (pt white-width 0)))
       (lbl-black-front-lower-alt (make-text-array '("[C♯]" "[E♭]" nil "[F♯]" "[G♯]" "[B♭]")
                                                   (pt white-width
                                                       (+ (- alt-shift)
                                                          white-front
                                                          (* 1/2 black-a-length)))
                                                   (pt white-width 0)))
       (lbl-black-back-lower (make-text-array '("3d♭" "3d♯" nil "3g♭" "3a♭" "3a♯")
                                              (pt white-width
                                                  (+ alt-shift
                                                     white-front
                                                     (+ black-a-length (* 1/2 black-b-length))))
                                              (pt white-width 0)))
       (lbl-black-back-lower-alt (make-text-array '("[D♭]" "[D♯]" nil "[G♭]" "[A♭]" "[A♯]")
                                                  (pt white-width
                                                      (+ (- alt-shift)
                                                         white-front
                                                         (+ black-a-length (* 1/2 black-b-length))))
                                              (pt white-width 0)))
       (lbl-tastini-lower (make-text-array '(nil nil "3e♯" nil nil nil "3b♯" )
                                           (pt white-width
                                               (- (+ alt-shift
                                                     white-front
                                                     (+ black-a-length black-b-length))
                                                  (* 1/2 tastino-length)))
                                           (pt white-width 0)))
       (lbl-tastini-lower-alt (make-text-array '(nil nil "[E♯]" nil nil nil "[B♯]" )
                                               (pt white-width
                                                   (- (+ (- alt-shift)
                                                         white-front
                                                         (+ black-a-length black-b-length))
                                                      (* 1/2 tastino-length)))
                                               (pt white-width 0)))
       (lbl-white-upper (make-text-array '("4c" "4d" "4e" "4f" "4g" "4a" "4b")
                                         (cp white-shift-pt origin (pt 0 alt-shift))
                                         (pt white-width 0)))
       (lbl-white-upper-alt (make-text-array '("[C’]" "[D’]" "[E’]" "[F’]" "[G’]" "[A’]" "[B♮’]")
                                             (cp white-shift-pt origin (pt 0 (- alt-shift)))
                                             (pt white-width 0)))
       (lbl-black-front-upper (make-text-array '("5d♭" "5e♭" nil "5g♭" "5a♭" "5b♭")
                                               (pt white-width
                                                   (+ alt-shift
                                                      white-front (* 1/2 black-a-length)))
                                               (pt white-width 0)))
       (lbl-black-front-upper-alt (make-text-array '("[D♭’]" "[E♭’]" nil "[F♯’]" "[A♭’]" "[B♭’]")
                                                   (pt white-width
                                                       (+ (- alt-shift)
                                                          white-front (* 1/2 black-a-length)))
                                                   (pt white-width 0)))
       (lbl-black-back-upper (make-text-array '("6b♯" "6d♯" nil "6e♯" "6g♯" "6a♯")
                                              (pt white-width
                                                  (+ alt-shift
                                                     white-front
                                                     (+ black-a-length (* 1/2 black-b-length))))
                                              (pt white-width 0)))
       (lbl-black-back-upper-alt (make-text-array '("[B♯’]" "[D♯’]" nil "[E♯’]" "[G♯’]" "[A♯’]")
                                                  (pt white-width
                                                      (+ (- alt-shift)
                                                         white-front
                                                         (+ black-a-length (* 1/2 black-b-length))))
                                                  (pt white-width 0)))
       (keyboard-labels (gr (list (cp lbl-white-lower origin anchor)
                                  (cp lbl-white-lower-alt origin anchor)
                                  (cp lbl-black-front-lower origin anchor)
                                  (cp lbl-black-front-lower-alt origin anchor)
                                  (cp lbl-black-back-lower origin anchor)
                                  (cp lbl-black-back-lower-alt origin anchor)
                                  (cp lbl-tastini-lower origin anchor)
                                  (cp lbl-tastini-lower-alt origin anchor)
                                  (cp lbl-white-upper origin (above anchor manual-separation))
                                  (cp lbl-white-upper-alt origin (above anchor manual-separation))
                                  (cp lbl-black-front-upper origin (above anchor manual-separation))
                                  (cp lbl-black-front-upper-alt origin (above anchor manual-separation))
                                  (cp lbl-black-back-upper origin (above anchor manual-separation))
                                  (cp lbl-black-back-upper-alt origin (above anchor manual-separation))
                                  ))))
  (draw-with-multiple-backends (list btikz) (list keyboard keyboard-labels))
  (compile-tikz btikz))


;; Custom keyboard Arciorgano, für Martin Kirnbauer, Mai 2024

(let* ((btikz (make-backend-tikz :filename "vicentino-arciorgano-notenames.tex"))
       (white-width 10)
       (white-width-2 (* 1/2 white-width))
       (white-front 12)
       (white-front-2 (* 1/2 white-front))
       (black-width 7)
       (black-width-2 (* 1/2 black-width))
       (black-a-length 9)
       (black-b-length 9)
       (tastino-width 5)
       (tastino-width-2 (* 1/2 tastino-width))
       (tastino-length 9)
       (origin (pt 0 0))
       (anchor (pt 12 12))
       (air .3)
       (white-shift-pt (pt white-width-2 white-front-2))
       (manual-separation (+ white-front black-a-length black-b-length air))
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
       (octave-c-c-lower (gr (list
                              (cp right-tastino-shape origin anchor)
                              (cp black-pair origin anchor)
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
                              (cp left-tastino-shape origin (right-of anchor (* 6 white-width)))
                              (cp first-tastino anchor (right-of anchor (* 4 white-width)))
                              )))
       (octave-c-c-upper (gr (list
                              (cp lowest-shape origin anchor)
                              (cp black-pair origin anchor)
                              (cp mid-shape origin (right-of anchor white-width))
                              (cp black-pair origin (right-of anchor white-width))
                              (cp highest-shape origin (right-of anchor (* 2 white-width)))
                              (cp lowest-shape origin (right-of anchor (* 3 white-width)))
                              (cp black-pair origin (right-of anchor (* 3 white-width)))
                              (cp mid-shape origin (right-of anchor (* 4 white-width)))
                              (cp black-pair origin (right-of anchor (* 4 white-width)))
                              (cp mid-shape origin (right-of anchor (* 5 white-width)))
                              (cp black-pair origin (right-of anchor (* 5 white-width)))
                              (cp highest-shape origin (right-of anchor (* 6 white-width)))
                              )))
       (keyboard (gr (list octave-c-c-lower
                           (cp octave-c-c-upper anchor (above anchor manual-separation)))))
       (lbl-white-lower (make-text-array '("C" "D" "E" "F" "G" "A" "B♮")
                                         white-shift-pt (pt white-width 0)))
       (lbl-black-front-lower (make-text-array '("C♯" "E♭" nil "F♯" "G♯" "B♭")
                                               (pt white-width (+ white-front (* 1/2 black-a-length)))
                                               (pt white-width 0)))
       (lbl-black-back-lower (make-text-array '("D♭" "D♯" nil "G♭" "A♭" "A♯")
                                              (pt white-width (+ white-front (+ black-a-length
                                                                                (* 1/2 black-b-length))))
                                              (pt white-width 0)))
       (lbl-tastini-lower (make-text-array '(nil nil "E♯" nil nil nil "B♯" )
                                           (pt white-width (- (+ white-front (+ black-a-length black-b-length))
                                                              (* 1/2 tastino-length)))
                                           (pt white-width 0)))
       (lbl-white-upper (make-text-array '("Ċ" "Ḋ" "Ė" "Ḟ" "Ġ" "Ȧ" "Ḃ♮")
                                         white-shift-pt (pt white-width 0)))
       (lbl-black-front-upper (make-text-array '("Ḋ♭" "Ė♭" nil "Ġ♭" "Ȧ♭" "Ḃ♭")
                                               (pt white-width (+ white-front (* 1/2 black-a-length)))
                                               (pt white-width 0)))
       (lbl-black-back-upper (make-text-array '("D'" "E'" nil "G'" "A'" "B♮'")
                                              (pt white-width (+ white-front (+ black-a-length
                                                                                (* 1/2 black-b-length))))
                                              (pt white-width 0)))
       (keyboard-labels (gr (list (cp lbl-white-lower origin anchor)
                                  (cp lbl-black-front-lower origin anchor)
                                  (cp lbl-black-back-lower origin anchor)
                                  (cp lbl-tastini-lower origin anchor)
                                  (cp lbl-white-upper origin (above anchor manual-separation))
                                  (cp lbl-black-front-upper origin (above anchor manual-separation))
                                  (cp lbl-black-back-upper origin (above anchor manual-separation))
                                  ))))
  (draw-with-multiple-backends (list btikz) (list keyboard keyboard-labels))
  (compile-tikz btikz))



;; Studio31-Publikation, Artikel «Das apaptiv-reine System»

(let* ((btikz-cmaj (make-backend-tikz :filename "vicentino-c-maj.tex"))
       (btikz-prog (make-backend-tikz :filename "vicentino-prog.tex"))
       (btikz (make-backend-tikz :filename "vicentino-num-c-c.tex"))
       (white-width 10)
       (white-width-2 (* 1/2 white-width))
       (white-front 12)
       (white-front-2 (* 1/2 white-front))
       (black-width 7)
       (black-width-2 (* 1/2 black-width))
       (black-a-length 9)
       (black-b-length 9)
       (tastino-width 5)
       (tastino-width-2 (* 1/2 tastino-width))
       (tastino-length 9)
       (origin (pt 0 0))
       (anchor (pt 12 12))
       (air .3)
       (white-shift-pt (pt white-width-2 white-front-2))
       (manual-separation (+ white-front black-a-length black-b-length air))
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
       (octave-c-c-lower (gr (list
                              (cp right-tastino-shape origin anchor)
                              (cp black-pair origin anchor)
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
                              (cp left-tastino-shape origin (right-of anchor (* 6 white-width)))
                              (cp first-tastino anchor (right-of anchor (* 4 white-width)))
                              )))
       (octave-c-c-upper (gr (list
                              (cp lowest-shape origin anchor)
                              (cp black-pair origin anchor)
                              (cp mid-shape origin (right-of anchor white-width))
                              (cp black-pair origin (right-of anchor white-width))
                              (cp highest-shape origin (right-of anchor (* 2 white-width)))
                              (cp lowest-shape origin (right-of anchor (* 3 white-width)))
                              (cp black-pair origin (right-of anchor (* 3 white-width)))
                              (cp mid-shape origin (right-of anchor (* 4 white-width)))
                              (cp black-pair origin (right-of anchor (* 4 white-width)))
                              (cp mid-shape origin (right-of anchor (* 5 white-width)))
                              (cp black-pair origin (right-of anchor (* 5 white-width)))
                              (cp highest-shape origin (right-of anchor (* 6 white-width)))
                              )))
       (keyboard (gr (list octave-c-c-lower
                           (cp octave-c-c-upper anchor (above anchor manual-separation)))))
       (lbl-white-lower (make-text-array '("C\\textsubscript{\\scriptsize I}" "D\\textsubscript{\\scriptsize I}" "E\\textsubscript{\\scriptsize I}" "F\\textsubscript{\\scriptsize I}" "G\\textsubscript{\\scriptsize I}" "A\\textsubscript{\\scriptsize I}" "B\\textsubscript{\\scriptsize I}")
                                         white-shift-pt (pt white-width 0)))
       (lbl-black-front-lower (make-text-array '("D\\textsubscript{\\scriptsize II}" "E\\textsubscript{\\scriptsize II}" nil "G\\textsubscript{\\scriptsize II}" "A\\textsubscript{\\scriptsize II}" "B\\textsubscript{\\scriptsize II}")
                                               (pt white-width (+ white-front (* 1/2 black-a-length)))
                                               (pt white-width 0)))
       (lbl-black-back-lower (make-text-array '("D\\textsubscript{\\scriptsize III}" "E\\textsubscript{\\scriptsize III}" nil "G\\textsubscript{\\scriptsize III}" "A\\textsubscript{\\scriptsize III}" "B\\textsubscript{\\scriptsize III}")
                                              (pt white-width (+ white-front (+ black-a-length
                                                                                (* 1/2 black-b-length))))
                                              (pt white-width 0)))
       (lbl-tastini-lower (make-text-array '(nil nil "F\\textsubscript{\\scriptsize III}" nil nil nil "C\\textsubscript{\\scriptsize III}" )
                                           (pt white-width (- (+ white-front (+ black-a-length black-b-length))
                                                              (* 1/2 tastino-length)))
                                           (pt white-width 0)))
       (lbl-white-upper (make-text-array '("C\\textsubscript{\\scriptsize IV}" "D\\textsubscript{\\scriptsize IV}" "E\\textsubscript{\\scriptsize IV}" "F\\textsubscript{\\scriptsize IV}" "G\\textsubscript{\\scriptsize IV}" "A\\textsubscript{\\scriptsize IV}" "B\\textsubscript{\\scriptsize IV}")
                                         white-shift-pt (pt white-width 0)))
       (lbl-black-front-upper (make-text-array '("D\\textsubscript{\\scriptsize V}" "E\\textsubscript{\\scriptsize V}" nil "G\\textsubscript{\\scriptsize V}" "A\\textsubscript{\\scriptsize V}" "B\\textsubscript{\\scriptsize V}")
                                               (pt white-width (+ white-front (* 1/2 black-a-length)))
                                               (pt white-width 0)))
       (lbl-black-back-upper (make-text-array '("D\\textsubscript{\\scriptsize VI}" "E\\textsubscript{\\scriptsize VI}" nil "G\\textsubscript{\\scriptsize VI}" "A\\textsubscript{\\scriptsize VI}" "B\\textsubscript{\\scriptsize VI}")
                                              (pt white-width (+ white-front (+ black-a-length
                                                                                (* 1/2 black-b-length))))
                                              (pt white-width 0)))
       (keyboard-labels (gr (list (cp lbl-white-lower origin anchor)
                                  (cp lbl-black-front-lower origin anchor)
                                  (cp lbl-black-back-lower origin anchor)
                                  (cp lbl-tastini-lower origin anchor)
                                  (cp lbl-white-upper origin (above anchor manual-separation))
                                  (cp lbl-black-front-upper origin (above anchor manual-separation))
                                  (cp lbl-black-back-upper origin (above anchor manual-separation))
                                  )))
       (c-pressed (circ white-width-2 white-front-2 1 :style-update '(:fill :fill)))
       (e-pressed (circ (+ white-width-2 (* 2 white-width)) white-front-2 1 :style-update '(:fill :fill)))
       (gg-pressed (circ (+ white-width-2 (* 4 white-width))
                         (+ white-front-2 manual-separation) 1 :style-update '(:fill :fill)))
       (bb♮-pressed (circ (+ white-width-2 (* 6 white-width))
                          (+ white-front-2 manual-separation) 1 :style-update '(:fill :fill)))
       (dis-pressed (circ (* 2 white-width) (+ manual-separation white-front black-a-length (* 1/2 black-b-length))
                          1 :style-update '(:fill :none)))
       (gis-pressed (circ (* 5 white-width) (+ white-front (* 1/2 black-a-length)) 1
                          :style-update '(:fill :none)))
       (his-pressed (circ (* 7 white-width) (- (+ white-front black-a-length black-b-length)
                                               (* 1/2 tastino-length))
                          1 :style-update '(:fill :none)))
       (c-chord (gr (list (cp c-pressed origin anchor)
                          (cp e-pressed origin anchor)
                          (cp gg-pressed origin anchor))))
       (e-chord (gr (list (cp e-pressed origin anchor)
                          (cp gg-pressed origin anchor)
                          (cp bb♮-pressed origin anchor))))
       (gis-chord (gr (list (cp gis-pressed origin anchor)
                            (cp his-pressed origin anchor)
                            (cp dis-pressed origin anchor)))))

  (draw-with-multiple-backends (list btikz)
                               (list keyboard keyboard-labels))
  (draw-with-multiple-backends (list btikz-cmaj)
                               (list keyboard c-chord))
  (draw-with-multiple-backends (list btikz-prog)
                               (list keyboard e-chord gis-chord))
  (compile-tikz btikz)
  (compile-tikz btikz-cmaj)
  (compile-tikz btikz-prog)
  )



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





;; Arciorgano keyboard, with standard shorthand notenames, for the Vicentino21 edition
;; (Editionsrichtlinien). Key mapping for 'tuning1'.

(let* ((btikz (make-backend-tikz :filename "vicentino21-arciorgano-shorthand-tuning1.tex"))
       (white-width 10)
       (white-width-2 (* 1/2 white-width))
       (white-front 12)
       (white-front-2 (* 1/2 white-front))
       (black-width 7)
       (black-width-2 (* 1/2 black-width))
       (black-a-length 9)
       (black-b-length 9)
       (tastino-width 5)
       (tastino-width-2 (* 1/2 tastino-width))
       (tastino-length 9)
       (origin (pt 0 0))
       (anchor (pt 12 12))
       (air .3)
       (white-shift-pt (pt white-width-2 white-front-2))
       (manual-separation (+ white-front black-a-length black-b-length air))
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
       (octave-c-c-lower (gr (list
                              (cp right-tastino-shape origin anchor)
                              (cp black-pair origin anchor)
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
                              (cp left-tastino-shape origin (right-of anchor (* 6 white-width)))
                              (cp first-tastino anchor (right-of anchor (* 4 white-width)))
                              )))
       (octave-c-c-upper (gr (list
                              (cp lowest-shape origin anchor)
                              (cp black-pair origin anchor)
                              (cp mid-shape origin (right-of anchor white-width))
                              (cp black-pair origin (right-of anchor white-width))
                              (cp highest-shape origin (right-of anchor (* 2 white-width)))
                              (cp lowest-shape origin (right-of anchor (* 3 white-width)))
                              (cp black-pair origin (right-of anchor (* 3 white-width)))
                              (cp mid-shape origin (right-of anchor (* 4 white-width)))
                              (cp black-pair origin (right-of anchor (* 4 white-width)))
                              (cp mid-shape origin (right-of anchor (* 5 white-width)))
                              (cp black-pair origin (right-of anchor (* 5 white-width)))
                              (cp highest-shape origin (right-of anchor (* 6 white-width)))
                              )))
       (keyboard (gr (list octave-c-c-lower
                           (cp octave-c-c-upper anchor (above anchor manual-separation)))))
       (lbl-white-lower (make-text-array '("C" "D" "E" "F" "G" "A" "B♮")
                                   white-shift-pt (pt white-width 0)))
       (lbl-black-front-lower (make-text-array '("C♯" "E♭" nil "F♯" "G♯" "B♭")
                                         (pt white-width (+ white-front (* 1/2 black-a-length)))
                                         (pt white-width 0)))
       (lbl-black-back-lower (make-text-array '("D♭" "D♯" nil "G♭" "A♭" "A♯")
                                        (pt white-width (+ white-front (+ black-a-length
                                                                          (* 1/2 black-b-length))))
                                        (pt white-width 0)))
       (lbl-tastini-lower (make-text-array '(nil nil "E♯" nil nil nil "B♯" )
                                     (pt white-width (- (+ white-front (+ black-a-length black-b-length))
                                                        (* 1/2 tastino-length)))
                                     (pt white-width 0)))
       (lbl-white-upper (make-text-array '("Ċ" "Ḋ" "Ė" "Ḟ" "Ġ" "Ȧ" "Ḃ♮")
                                         white-shift-pt (pt white-width 0)))
       (lbl-black-front-upper (make-text-array '("Ḋ♭" "Ė♭" nil "Ġ♭" "Ȧ♭" "Ḃ♭")
                                         (pt white-width (+ white-front (* 1/2 black-a-length)))
                                         (pt white-width 0)))
       (lbl-black-back-upper (make-text-array '("D'" "E'" nil "G'" "A'" "B♮'")
                                        (pt white-width (+ white-front (+ black-a-length
                                                                          (* 1/2 black-b-length))))
                                        (pt white-width 0)))
       (keyboard-labels (gr (list (cp lbl-white-lower origin anchor)
                                  (cp lbl-black-front-lower origin anchor)
                                  (cp lbl-black-back-lower origin anchor)
                                  (cp lbl-tastini-lower origin anchor)
                                  (cp lbl-white-upper origin (above anchor manual-separation))
                                  (cp lbl-black-front-upper origin (above anchor manual-separation))
                                  (cp lbl-black-back-upper origin (above anchor manual-separation))
                                  ))))
  (draw-with-multiple-backends (list btikz) (list keyboard keyboard-labels))
  (compile-tikz btikz))


;; Arciorgano keyboard, with standard shorthand notenames, for the Vicentino21 edition
;; (Editionsrichtlinien). Key mapping for 'tuning2'.

(let* ((btikz (make-backend-tikz :filename "vicentino21-arciorgano-shorthand-tuning2.tex"))
       (white-width 10)
       (white-width-2 (* 1/2 white-width))
       (white-front 12)
       (white-front-2 (* 1/2 white-front))
       (black-width 7)
       (black-width-2 (* 1/2 black-width))
       (black-a-length 9)
       (black-b-length 9)
       (tastino-width 5)
       (tastino-width-2 (* 1/2 tastino-width))
       (tastino-length 9)
       (origin (pt 0 0))
       (anchor (pt 12 12))
       (air .3)
       (white-shift-pt (pt white-width-2 white-front-2))
       (manual-separation (+ white-front black-a-length black-b-length air))
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
       (octave-c-c-lower (gr (list
                              (cp right-tastino-shape origin anchor)
                              (cp black-pair origin anchor)
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
                              (cp left-tastino-shape origin (right-of anchor (* 6 white-width)))
                              (cp first-tastino anchor (right-of anchor (* 4 white-width)))
                              )))
       (octave-c-c-upper (gr (list
                              (cp lowest-shape origin anchor)
                              (cp black-pair origin anchor)
                              (cp mid-shape origin (right-of anchor white-width))
                              (cp black-pair origin (right-of anchor white-width))
                              (cp highest-shape origin (right-of anchor (* 2 white-width)))
                              (cp lowest-shape origin (right-of anchor (* 3 white-width)))
                              (cp black-pair origin (right-of anchor (* 3 white-width)))
                              (cp mid-shape origin (right-of anchor (* 4 white-width)))
                              (cp black-pair origin (right-of anchor (* 4 white-width)))
                              (cp mid-shape origin (right-of anchor (* 5 white-width)))
                              (cp black-pair origin (right-of anchor (* 5 white-width)))
                              (cp highest-shape origin (right-of anchor (* 6 white-width)))
                              )))
       (keyboard (gr (list octave-c-c-lower
                           (cp octave-c-c-upper anchor (above anchor manual-separation)))))
       (lbl-white-lower (make-text-array '("C" "D" "E" "F" "G" "A" "B♮")
                                   white-shift-pt (pt white-width 0)))
       (lbl-black-front-lower (make-text-array '("C♯" "E♭" nil "F♯" "G♯" "B♭")
                                         (pt white-width (+ white-front (* 1/2 black-a-length)))
                                         (pt white-width 0)))
       (lbl-black-back-lower (make-text-array '("D♭" "D♯" nil "G♭" "A♭" "A♯")
                                        (pt white-width (+ white-front (+ black-a-length
                                                                          (* 1/2 black-b-length))))
                                        (pt white-width 0)))
       (lbl-tastini-lower (make-text-array '(nil nil "E♯" nil nil nil "B♯" )
                                     (pt white-width (- (+ white-front (+ black-a-length black-b-length))
                                                        (* 1/2 tastino-length)))
                                     (pt white-width 0)))
       (lbl-white-upper (make-text-array '("C'" "D'" "E'" "F'" "G'" "A'" "B♮'")
                                         white-shift-pt (pt white-width 0)))
       (lbl-black-front-upper (make-text-array '("C♯'" "E♭'" nil "F♯'" "G♯'" "B♭'")
                                         (pt white-width (+ white-front (* 1/2 black-a-length)))
                                         (pt white-width 0)))
       (lbl-black-back-upper (make-text-array '("D♭'" "D♯'" nil "G♭'" "A♭'" "A♯'")
                                        (pt white-width (+ white-front (+ black-a-length
                                                                          (* 1/2 black-b-length))))
                                        (pt white-width 0)))
       (keyboard-labels (gr (list (cp lbl-white-lower origin anchor)
                                  (cp lbl-black-front-lower origin anchor)
                                  (cp lbl-black-back-lower origin anchor)
                                  (cp lbl-tastini-lower origin anchor)
                                  (cp lbl-white-upper origin (above anchor manual-separation))
                                  (cp lbl-black-front-upper origin (above anchor manual-separation))
                                  (cp lbl-black-back-upper origin (above anchor manual-separation))
                                  ))))
  (draw-with-multiple-backends (list btikz) (list keyboard keyboard-labels))
  (compile-tikz btikz))




;; Standard keyboard layout for the Clavemusicum Omnitonum, for a short document about the tuning
;; process of the instrument

(let* ((btikz (make-backend-tikz :filename "clavemusicum-notenames.tex"))
       (white-width 10)
       (white-width-2 (* 1/2 white-width))
       (white-front 16)
       (white-front-2 (* 1/2 white-front))
       (black-width 6)
       (black-width-2 (* 1/2 black-width))
       (black-a-length 10)
       (black-b-length 10)
       (black-c-length 10)
       (black-d-length 10)
       (black-total-length (+ black-a-length black-b-length black-c-length black-d-length))
       (tastino-width 4)
       (tastino-width-2 (* 1/2 tastino-width))
       (tastino-a-length 13)
       (tastino-b-length 13)
       (origin (pt 0 0))
       (anchor (pt 12 12))
       (air .3)
       (white-shift-pt (pt white-width-2 white-front-2))
       ;; (lowest-shape (ln-shape origin (list white-width
       ;;                                      white-front
       ;;                                      (- black-width-2)
       ;;                                      black-total-length
       ;;                                      (- (- white-width black-width-2)))
       ;;                         :shift air :shift-pt white-shift-pt))
       ;; (highest-shape (ln-shape origin (list white-width
       ;;                                       (+ white-front black-total-length)
       ;;                                       (- (- white-width black-width-2))
       ;;                                       (- black-total-length)
       ;;                                       (- black-width-2))
       ;;                          :shift air :shift-pt white-shift-pt))
       (mid-shape (ln-shape origin (list white-width
                                         white-front
                                         (- black-width-2)
                                         black-total-length
                                         (- (- white-width black-width))
                                         (- black-total-length)
                                         (- black-width-2))
                            :shift air :shift-pt white-shift-pt))
       (left-tastino-shape (ln-shape origin (list white-width
                                                  (- (+ white-front black-total-length)
                                                     (+ tastino-a-length tastino-b-length))
                                                  (- tastino-width-2)
                                                  (+ tastino-a-length tastino-b-length)
                                                  (- (- white-width black-width-2 tastino-width-2))
                                                  (- black-total-length)
                                                  (- black-width-2))
                                     :shift air :shift-pt white-shift-pt))
       (right-tastino-shape (ln-shape origin (list white-width
                                                   white-front
                                                   (- black-width-2)
                                                   black-total-length
                                                   (- (- white-width black-width-2 tastino-width-2))
                                                   (- (+ tastino-a-length tastino-b-length))
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
       (black-c (ln-shape origin (list black-width
                                       black-c-length
                                       (- black-width))
                          :shift air :shift-pt (pt black-width-2 (* 1/2 black-c-length))))
       (black-d (ln-shape origin (list black-width
                                       black-d-length
                                       (- black-width))
                          :shift air :shift-pt (pt black-width-2 (* 1/2 black-d-length))))
       (tastino-a (ln-shape origin (list tastino-width
                                       tastino-a-length
                                       (- tastino-width))
                          :shift air :shift-pt (pt tastino-width-2 (* 1/2 tastino-a-length))))
       (tastino-b (ln-shape origin (list tastino-width
                                       tastino-b-length
                                       (- tastino-width))
                          :shift air :shift-pt (pt tastino-width-2 (* 1/2 tastino-b-length))))
       (tastino-column (gr (list (cp tastino-a origin
                                     (add anchor (pt (- (* 3 white-width) tastino-width-2)
                                                     (- (+ white-front black-total-length)
                                                        tastino-a-length))))
                                 (cp tastino-b origin
                                     (add anchor (pt (- (* 3 white-width) tastino-width-2)
                                                     (- (+ white-front black-total-length)
                                                        (+ tastino-a-length tastino-b-length))))))))
       (black-column (gr (list (cp black-a origin (pt (- white-width black-width-2)
                                                      white-front))
                               (cp black-b origin (pt (- white-width
                                                         black-width-2)
                                                      (+ white-front black-a-length)))
                               (cp black-c origin (pt (- white-width
                                                         black-width-2)
                                                      (+ white-front black-a-length black-b-length)))
                               (cp black-d origin (pt (- white-width
                                                         black-width-2)
                                                      (+ white-front
                                                         black-a-length
                                                         black-b-length
                                                         black-c-length))))))
       (keyboard (gr (list
                      (cp right-tastino-shape origin anchor)
                      (cp black-column origin anchor)
                      (cp mid-shape origin (right-of anchor white-width))
                      (cp black-column origin (right-of anchor white-width))
                      (cp left-tastino-shape origin (right-of anchor (* 2 white-width)))
                      tastino-column
                      (cp right-tastino-shape origin (right-of anchor (* 3 white-width)))
                      (cp black-column origin (right-of anchor (* 3 white-width)))
                      (cp mid-shape origin (right-of anchor (* 4 white-width)))
                      (cp black-column origin (right-of anchor (* 4 white-width)))
                      (cp mid-shape origin (right-of anchor (* 5 white-width)))
                      (cp black-column origin (right-of anchor (* 5 white-width)))
                      (cp left-tastino-shape origin (right-of anchor (* 6 white-width)))
                      (cp tastino-column anchor (right-of anchor (* 4 white-width))))))
       (lbl-white (make-text-array '("C" "D" "E" "F" "G" "A" "B♮")
                                   (add (pt air 0) white-shift-pt) (pt white-width 0)))
       (lbl-black-a (make-text-array '("C♯" "E♭" nil "F♯" "G♯" "B♭")
                                     (pt (+ air white-width)
                                         (+ white-front (* 1/2 black-a-length)))
                                     (pt white-width 0)))
       (lbl-black-b (make-text-array '("D♭" "D♯" nil "G♭" "A♭" "A♯")
                                     (pt (+ air white-width)
                                         (+ white-front (+ black-a-length
                                                           (* 1/2 black-b-length))))
                                     (pt white-width 0)))
       (lbl-black-c (make-text-array '("C♯♯" "E♭♭" nil "F♯♯" "G♯♯" "B♭♭")
                                     (pt (+ air white-width)
                                         (+ white-front (+ black-a-length
                                                           black-b-length
                                                           (* 1/2 black-c-length))))
                                     (pt white-width 0)))
       (lbl-black-d (make-text-array '("D♭♭" "D♯♯" nil "G♭♭" "A♭♭" "A♯♯")
                                     (pt (+ air white-width)
                                         (+ white-front (+ black-a-length
                                                           black-b-length
                                                           black-c-length
                                                           (* 1/2 black-d-length))))
                                     (pt white-width 0)))
       (lbl-tastini-a (make-text-array '(nil nil "F♭" nil nil nil "C♭" )
                                       (pt (+ air white-width)
                                           (- (+ white-front (+ black-total-length))
                                              (* 1/2 tastino-b-length)))
                                       (pt white-width 0)))
       (lbl-tastini-b (make-text-array '(nil nil "E♯" nil nil nil "B♯" )
                                       (pt (+ air white-width)
                                           (- (+ white-front (+ black-total-length))
                                              tastino-b-length
                                              (* 1/2 tastino-a-length)))
                                       (pt white-width 0)))
       (keyboard-labels (gr (list (cp lbl-white origin anchor)
                                  (cp lbl-black-a origin anchor)
                                  (cp lbl-black-b origin anchor)
                                  (cp lbl-black-c origin anchor)
                                  (cp lbl-black-d origin anchor)
                                  (cp lbl-tastini-a origin anchor)
                                  (cp lbl-tastini-b origin anchor)))))
  (draw-with-multiple-backends (list btikz) (list keyboard keyboard-labels))
  (compile-tikz btikz))
