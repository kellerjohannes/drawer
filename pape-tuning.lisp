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

(let* ((piano-names (list :c♯ :d :e♭ :e :f :f♯ :g :g♯ :a :b♭ :b♮ :c))
       (arci-names (list :cʼ :c♯ :c♯ʼ
                             :d♭ :d♭ʼ :d :dʼ :d♯ :d♯ʼ
                             :e♭ :e♭ʼ :e :eʼ :e♯
                         :f :fʼ :f♯ :f♯ʼ
                         :g♭ :g♭ʼ :g :gʼ :g♯ :g♯ʼ
                         :a♭ :a♭ʼ :a :aʼ :a♯ :a♯ʼ
                         :b♭ :b♭ʼ :b♮ :b♮ʼ :b♯ :c))
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
                                                       (remove-arci-gaps
                                                        (expand-interval-list arci-names 4)
                                                        '((:cʼ . 0) (:c♯ʼ . 0) (:d♭ʼ . 0)
                                                          (:dʼ . 0) (:d♯ʼ . 0) (:e♭ʼ . 0)
                                                          (:eʼ . 0) (:b♯ . 3)))))
                                     (cons "C$^1$" (remove-arci-gaps
                                                    (expand-name-list arci-names 4)
                                                    '("Cʼ$^{1}$" "C♯ʼ$^{1}$" "D♭ʼ$^{1}$"
                                                      "Dʼ$^{1}$" "D♯ʼ$^{1}$" "E♭ʼ$^{1}$"
                                                      "Eʼ$^{1}$" "B♯$^{4}$")))
                                     (pt 10 (* (vicentino-tunings:interval-size :tuning1 :c :up :d)
                                               0.35))
                                     0.35
                                     :main-label "Arciorgano"))
       (scale-clave (make-scale (cons 1/1 (mapcar (lambda (pitch)
                                                    (* (vicentino-tunings:interval :31ed2
                                                                                   :c
                                                                                   :up
                                                                                   (car pitch))
                                                       (expt 2 (cdr pitch))))
                                                  (expand-interval-list clave-names 4)))
                                (cons "C$^1$" (expand-name-list clave-names 4))
                                (pt 20 0)
                                0.35
                                :main-label "Clavemusicum"))
       (scale-piano (make-scale (cons 1/1 (mapcar (lambda (pitch)
                                                    (* (vicentino-tunings:interval :12ed2
                                                                                   :c
                                                                                   :up
                                                                                   (car pitch))
                                                       (expt 2 (cdr pitch))))
                                                  (expand-interval-list piano-names 4)))
                                (cons "C$^1$" (expand-name-list piano-names 4))
                                (pt 0 0)
                                0.35
                                :main-label "Piano"))
       (helper-lines-endpoint 30)
       (pape-padding -13)
       (scale-pape-g (make-spectrum (pt (* 1 pape-padding) 0) 0.35 16 "G"
                                    (vicentino-tunings:interval :12ed2 :c :up :g)
                                    helper-lines-endpoint))
       (scale-pape-b♭ (make-spectrum (pt (* 2 pape-padding) 0) 0.35 16 "B♭"
                                     (vicentino-tunings:interval :12ed2 :c :down :b♭)
                                     helper-lines-endpoint))
       (scale-pape-c (make-spectrum (pt (* 3 pape-padding) 0) 0.35 16 "C" 1/1
                                    helper-lines-endpoint))
       (scale-pape-d (make-spectrum (pt (* 4 pape-padding) 0) 0.35 16 "D"
                                    (vicentino-tunings:interval :12ed2 :c :up :d)
                                    helper-lines-endpoint))
       (scale-pape-a (make-spectrum (pt (* 5 pape-padding) 0) 0.35 16 "A"
                                    (vicentino-tunings:interval :12ed2 :c :up :a)
                                    helper-lines-endpoint))
       (scale-pape-b♮ (make-spectrum (pt (* 6 pape-padding) 0) 0.35 16 "B♮"
                                     (vicentino-tunings:interval :12ed2 :c :down :b♮)
                                     helper-lines-endpoint))
       (scale-pape-e (make-spectrum (pt (* 7 pape-padding) 0) 0.35 16 "E"
                                    (vicentino-tunings:interval :12ed2 :c :up :e)
                                    helper-lines-endpoint))
       (scale-pape-f♯ (make-spectrum (pt (* 8 pape-padding) 0) 0.35 16 "F♯"
                                     (vicentino-tunings:interval :12ed2 :c :up :f♯)
                                     helper-lines-endpoint))
       (scale-pape-d♭ (make-spectrum (pt (* 9 pape-padding) 0) 0.35 16 "D♭"
                                     (vicentino-tunings:interval :12ed2 :c :up :d♭)
                                     helper-lines-endpoint))
       (scale-pape-e♭ (make-spectrum (pt (* 10 pape-padding) 0) 0.35 16 "E♭"
                                     (vicentino-tunings:interval :12ed2 :c :up :e♭)
                                     helper-lines-endpoint))
       (scale-pape-a♭ (make-spectrum (pt (* 11 pape-padding) 0) 0.35 16 "A♭"
                                     (vicentino-tunings:interval :12ed2 :c :up :a♭)
                                     helper-lines-endpoint))
       (scale-pape-f (make-spectrum (pt (* 12 pape-padding) 0) 0.35 16 "F"
                                    (vicentino-tunings:interval :12ed2 :c :up :f)
                                    helper-lines-endpoint))
       (btikz (make-backend-tikz :filename "studio31-pape-tuning.tex")))
  (draw-with-multiple-backends (list btikz) (list scale-pape-g
                                                  scale-pape-b♭
                                                  scale-pape-c
                                                  scale-pape-d
                                                  scale-pape-a
                                                  scale-pape-b♮
                                                  scale-pape-e
                                                  scale-pape-f♯
                                                  scale-pape-d♭
                                                  scale-pape-e♭
                                                  scale-pape-a♭
                                                  scale-pape-f
                                                  scale-piano
                                                  scale-arciorgano
                                                  scale-clave
                                                  ))
  (compile-tikz btikz))
