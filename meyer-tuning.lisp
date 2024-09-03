(in-package :drawer)

(ql:quickload :vicentino-tunings)

(defun make-scale (interval-list label-list origin y-scale &key helper-line-endpoint
                                                             (main-label ""))
  (let* ((tick-length 1.3)
         (label-offset 0.5)
         (point-list (mapcar (lambda (interval)
                               (add origin (pt 0 (* y-scale
                                                    (vicentino-tunings:ratio->length interval)))))
                             interval-list))
         (local-origin (pt 0 0))
         (helper-lines (when helper-line-endpoint
                         (gr (mapcar (lambda (point)
                                       (ln point
                                           (pt helper-line-endpoint (value (y point)))
                                           :style-update '(:line-type :dotted)))
                                     point-list))))
         (tick (ln local-origin (right-of local-origin tick-length)))
         (tick-gr (gr (mapcar (lambda (point)
                                (cp tick local-origin point))
                              point-list)))
         (label-gr (gr (mapcar (lambda (label point)
                                 (make-text label (left-of point label-offset) :h-align :right))
                               label-list
                               point-list)))
         (central-line (ln origin (first (sort (copy-list point-list)
                                               #'>
                                               :key (lambda (point)
                                                      (value (y point)))))))
         (main-label (make-text main-label (below origin 3) :v-align :top))
         (scale-list (gr (list central-line tick-gr label-gr main-label))))
    (if helper-line-endpoint
        (gr (list helper-lines scale-list))
        scale-list)))

(defun make-spectrum (origin y-scale number-of-partials prefix interval-offset helper-line-endpoint)
  (make-scale (loop for i from 1 to number-of-partials
                    collect i)
              (loop for i from 1 to number-of-partials
                    collect (format nil "(~a) ~a:~a" prefix i 1))
              (pt (value (x origin)) (+ (value (y origin))
                                        (* (vicentino-tunings:ratio->length interval-offset)
                                           y-scale)))
              y-scale
              :helper-line-endpoint helper-line-endpoint
              :main-label (format nil "Pape ~a" prefix)))

(defun expand-interval-list (notename-list octaves)
  (let ((result nil))
    (dotimes (i octaves result)
      (setf result (append result (mapcar (lambda (notename)
                                            (cons notename i))
                                          notename-list))))))

(defun remove-arci-gaps (pitch-list candidate-list)
  (let ((result pitch-list))
    (dolist (candidate candidate-list result)
      (setf result (remove-if (lambda (pitch)
                                (equal candidate pitch))
                              result)))))

(defun expand-name-list (notename-list octaves)
  (let ((result nil))
    (dotimes (i octaves result)
      (setf result (append result (mapcar (lambda (notename)
                                            (format nil "~a$^{~a}$" notename (if (eq notename :c)
                                                                                 (+ 2 i)
                                                                                 (1+ i))))
                                          notename-list))))))

(let* ((octave-range 2)
       (piano-names (list :c♯ :d :e♭ :e :f :f♯ :g :g♯ :a :b♭ :b♮ :c))
       (arci-names (list :cʼ :c♯ :c♯ʼ
                             :d♭ :d♭ʼ :d :dʼ :d♯ :d♯ʼ
                             :e♭ :e♭ʼ :e :eʼ :e♯
                         :f :fʼ :f♯ :f♯ʼ
                         :g♭ :g♭ʼ :g :gʼ :g♯ :g♯ʼ
                         :a♭ :a♭ʼ :a :aʼ :a♯ :a♯ʼ
                         :b♭ :b♭ʼ :b♮ :b♮ʼ :b♯ :c))
       ;; (arci-keynames (list :C♯1 :D♭1 :D1 :D♯1 :E♭1 :E1 :E♯1 :F1 :Ḟ1 :F♯1 :Ḟ♯1 :G♭1 :Ġ♭1 :G1 :Ġ1 :G♯1 :Ġ♯1 :A♭1 :Ȧ♭1 :A1 :Ȧ1 :A♯1 :Ȧ♯1 :B♭1 :Ḃ♭1 :B♮1 :Ḃ♮1 :B♯1 :C2 :Ċ2 :C♯2 :Ċ♯2 :D♭2 :Ḋ♭2 :D2 :Ḋ2 :D♯2 :Ḋ♯2 :E♭2 :Ė♭2 :E2 :Ė2 :E♯2 :F2 :Ḟ2 :F♯2 :Ḟ♯2 :G♭2 :Ġ♭2 :G2 :Ġ2 :G♯2 :Ġ♯2 :A♭2 :Ȧ♭2 :A2 :Ȧ2 :A♯2 :Ȧ♯2 :B♭2 :Ḃ♭2 :B♮2 :Ḃ♮2 :B♯2 :C3 :Ċ3 :C♯3 :Ċ♯3 :D♭3 :Ḋ♭3 :D3 :Ḋ3 :D♯3 :Ḋ♯3 :E♭3 :Ė♭3 :E3 :Ė3 :E♯3 :F3 :Ḟ3 :F♯3 :Ḟ♯3 :G♭3 :Ġ♭3 :G3 :Ġ3 :G♯3 :Ġ♯3 :A♭3 :Ȧ♭3 :A3 :Ȧ3 :A♯3 :Ȧ♯3 :B♭3 :Ḃ♭3 :B♮3 :Ḃ♮3 :B♯3 :C4 :Ċ4 :C♯4 :Ċ♯4 :D♭4 :Ḋ♭4 :D4 :Ḋ4 :D♯4 :Ḋ♯4 :E♭4 :Ė♭4 :E4 :Ė4 :E♯4 :F4 :Ḟ4 :F♯4 :Ḟ♯4 :G♭4 :Ġ♭4 :G4 :Ġ4 :G♯4 :Ġ♯4 :A♭4 :Ȧ♭4 :A4 :Ȧ4 :A♯4 :Ȧ♯4 :B♭4 :Ḃ♭4 :B♮4 :Ḃ♮4 :C5 :Ċ5))
       (clave-names (list :d♭♭ :c♯
                          :d♭ :c♯♯ :d :e♭♭ :d♯
                               :e♭ :d♯♯ :e :f♭ :e♯
                          :f :g♭♭ :f♯
                               :g♭ :f♯♯ :g :a♭♭ :g♯
                          :a♭ :g♯♯ :a :b♭♭ :a♯
                               :b♭ :a♯♯ :b♮ :c♭ :b♯ :c))
       (scale-arciorgano (make-scale (cons 1/1 (mapcar (lambda (pitch)
                                                         (* (vicentino-tunings:interval :tuning1
                                                                                        :c
                                                                                        :up
                                                                                        (car pitch))
                                                            (expt 2 (cdr pitch))))
                                                       (expand-interval-list arci-names octave-range)))
                                     (cons "C$^1$" (expand-name-list arci-names octave-range))
                                     (pt 30 (* (vicentino-tunings:interval-size :tuning1 :c :up :d)
                                               0.35))
                                     0.35
                                     :main-label "Arciorgano"))
       (scale-clave (make-scale (cons 1/1 (mapcar (lambda (pitch)
                                                    (* (vicentino-tunings:interval :31ed2
                                                                                   :c
                                                                                   :up
                                                                                   (car pitch))
                                                       (expt 2 (cdr pitch))))
                                                  (expand-interval-list clave-names octave-range)))
                                (cons "C$^1$" (expand-name-list clave-names octave-range))
                                (pt 40 0)
                                0.35
                                :main-label "Clavemusicum"))
       (scale-piano (make-scale (cons 1/1 (mapcar (lambda (pitch)
                                                    (* (vicentino-tunings:interval :12ed2
                                                                                   :c
                                                                                   :up
                                                                                   (car pitch))
                                                       (expt 2 (cdr pitch))))
                                                  (expand-interval-list piano-names octave-range)))
                                (cons "C$^1$" (expand-name-list piano-names octave-range))
                                (pt 20 (* (- (vicentino-tunings:ratio->length (expt 81/80 1/4))
                                             (vicentino-tunings:ratio->length (expt 531441/524288 1/12)))
                                          0.35))
                                0.35
                                :main-label "12ed2"))
       (pyth-names (list :d♭ :c♯ :d :e♭ :d♯ :e :e♯ :f :g♭ :f♯ :g :a♭ :g♯ :a :b♭ :a♯ :b♮ :b♯ :c))
       (scale-pythagorean (make-scale (cons 1/1 (mapcar (lambda (pitch)
                                                          (* (vicentino-tunings:interval :pyth
                                                                                       :c
                                                                                       :up
                                                                                       (car pitch))
                                                             (expt 2 (cdr pitch))))
                                                        (expand-interval-list pyth-names octave-range)))
                                      (cons "C" (expand-name-list pyth-names octave-range))
                                      (pt 10 (* (vicentino-tunings:ratio->length (expt 81/80 1/4))
                                                0.35))
                                      0.35
                                      :main-label "Pythagorean"))
       (meyer-tuning '(("XII" (0 0 0) 1024)
                       ("XII" (0 2 1) 1008)
                       ("XII" (3 0 0) 1000)
                       ("XI" (0 5 0) 972)
                       ("XI" (1 1 0) 960)
                       ("XI" (1 3 1) 945)
                       ("X" (2 2 0) 900)
                       ("X" (0 0 1) 896)
                       ("IX" (0 3 0) 864)
                       ("IX" (1 1 1) 840)
                       ("VIII" (1 4 0) 810)
                       ("VIII" (2 0 0) 800)
                       ("VII" (0 1 0) 768)
                       ("VII" (0 3 1) 756)
                       ("VII" (3 1 0) 750)
                       ("VI" (0 6 0) 729)
                       ("VI" (1 2 0) 720)
                       ("VI" (2 0 1) 700)
                       ("V" (2 3 0) 675)
                       ("V" (0 1 1) 672)
                       ("IV" (0 4 0) 648)
                       ("IV" (1 0 0) 640)
                       ("IV" (1 2 1) 630)
                       ("III" (2 1 0) 600)
                       ("II" (0 2 0) 576)
                       ("II" (0 4 1) 567)
                       ("II" (1 0 1) 560)
                       ("I" (1 3 0) 540)
                       ("I" (2 1 1) 525)))
       (helper-lines-endpoint 40)
       (scale-meyer (make-scale (mapcar (lambda (pitch)
                                          (let ((subscripts (second pitch)))
                                            (vicentino-tunings::simplify
                                             (* (expt 3 (second subscripts))
                                                (expt 5 (first subscripts))
                                                (expt 7 (third subscripts))))))
                                        meyer-tuning)
                                (mapcar (lambda (pitch)
                                          (let ((id (let ((subscripts (second pitch)))
                                                    (* (expt 3 (second subscripts))
                                                       (expt 5 (first subscripts))
                                                       (expt 7 (third subscripts))))))
                                            (format nil "~a ~a [~a]"
                                                    id
                                                    (first pitch)
                                                    (vicentino-tunings::simplify id)
                                                    )))
                                        meyer-tuning)
                                (pt 0 (* (vicentino-tunings:interval-size :tuning1 :c :up :f)
                                          0.35))
                                0.35
                                :helper-line-endpoint helper-lines-endpoint
                                :main-label "Meyer"))
       (btikz (make-backend-tikz :filename "studio31-meyer-tuning.tex")))
  (draw-with-multiple-backends (list btikz) (list scale-piano
                                                  ;; scale-custom-arciorgano
                                                  scale-arciorgano
                                                  scale-clave
                                                  scale-pythagorean
                                                  scale-meyer
                                                  ))
  (compile-tikz btikz))
