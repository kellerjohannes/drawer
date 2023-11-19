(in-package :drawer)

(ql:quickload :vicentino-tunings)

(defun make-scale (interval-list label-list origin y-scale)
  (let* ((tick-length 2)
         (label-offset 3)
         (point-list (mapcar (lambda (interval)
                               (add origin (pt 0 (* y-scale
                                                    (vicentino-tunings:ratio->length interval)))))
                             interval-list))
         (local-origin (pt 0 0))
         (tick (ln local-origin (right-of local-origin tick-length)))
         (tick-gr (gr (mapcar (lambda (point)
                              (cp tick local-origin point))
                            point-list)))
         (label-gr (gr (mapcar (lambda (label point)
                                 (make-text label (right-of point label-offset) :h-align :left))
                               label-list
                               point-list)))
         (central-line (ln origin (first (sort (copy-list point-list)
                                               #'>
                                               :key (lambda (point)
                                                      (value (y point))))))))
    (gr (list central-line tick-gr label-gr))))

(defun make-spectrum (origin y-scale))
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
       (clave-names (list :ċ :c♯
                          :d♭ :ḋ♭ :d :ḋ :d♯
                          :e♭ :ė♭ :e :ė :e♯
                          :f :ḟ :f♯
                          :g♭ :ġ♭ :g :ġ :g♯
                          :a♭ :ȧ♭ :a :ȧ :a♯
                          :b♭ :ḃ♭ :b♮ :ḃ♮ :b♯ :c))
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
                                     0.35))
       (scale-clave (make-scale (cons 1/1 (mapcar (lambda (pitch)
                                                         (* (vicentino-tunings:interval :tuning1
                                                                                        :c
                                                                                        :up
                                                                                        (car pitch))
                                                            (expt 2 (cdr pitch))))
                                                       (expand-interval-list clave-names 4)))
                                     (cons "C$^1$" (expand-name-list clave-names 4))
                                     (pt 20 0)
                                     0.35))
       (scale-piano (make-scale (cons 1/1 (mapcar (lambda (pitch)
                                                    (* (vicentino-tunings:interval :12ed2
                                                                                   :c
                                                                                   :up
                                                                                   (car pitch))
                                                       (expt 2 (cdr pitch))))
                                                  (expand-interval-list piano-names 4)))
                                (cons "C$^1$" (expand-name-list piano-names 4))
                                (pt 0 0)
                                0.35))
       (btikz (make-backend-tikz :filename "scale-test.tex")))
  (format t "~&~a" (length clave-names))
  (draw-with-multiple-backends (list btikz) (list scale-arciorgano scale-clave scale-piano))
  (compile-tikz btikz))
