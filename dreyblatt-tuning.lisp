(in-package :drawer)
(ql:quickload :vicentino-tunings)

(defun generate-magic-square (root-list)
  (let ((result nil))
    (dolist (x root-list)
      (dolist (y root-list)
        (push (* x y) result)))
    (sort (remove-duplicates result) #'<)))

(defparameter *dreyblatt-roots* (list 1 3 5 7 9 11))

(defparameter *dreyblatt-numbers* (generate-magic-square *dreyblatt-roots*))

(defun simplify (interval)
  (cond ((< interval 1) (simplify (* interval 2)))
        ((>= interval 2) (simplify (* interval 1/2)))
        (t interval)))

(defparameter *dreyblatt-octave* (mapcar #'simplify *dreyblatt-numbers*))


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



(let* ((btikz (make-backend-tikz :filename "dreyblatt-tuning.tex"))
      (arci-names (list :cʼ :c♯ :c♯ʼ
                            :d♭ :d♭ʼ :d :dʼ :d♯ :d♯ʼ
                            :e♭ :e♭ʼ :e :eʼ :e♯
                        :f :fʼ :f♯ :f♯ʼ
                        :g♭ :g♭ʼ :g :gʼ :g♯ :g♯ʼ
                        :a♭ :a♭ʼ :a :aʼ :a♯ :a♯ʼ
                        :b♭ :b♭ʼ :b♮ :b♮ʼ :b♯ :c))
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
                                    (pt 10 0)
                                    0.2
                                    :main-label "Arciorgano"))
      (dreyblatt-scale (make-scale (cons 2/1 *dreyblatt-octave*)
                                   (cons "(2)" *dreyblatt-numbers*)
                                   (pt 0 0) .2 :main-label "Dreyblatt")))
  (draw-with-multiple-backends (list btikz) (list scale-arciorgano dreyblatt-scale))
  (compile-tikz btikz))
