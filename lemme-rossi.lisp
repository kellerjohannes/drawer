(in-package :drawer)
(ql:quickload :vicentino-tunings)


(defun simplify (interval)
  (cond ((< interval 1) (simplify (* interval 2)))
        ((>= interval 2) (simplify (* interval 1/2)))
        (t interval)))


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


(defparameter *rossi-vicentino* (list 41472 40554 39657 38781 37923 37084 36264 35463 34679 33912 33162 32429 31712 31011 30325 29655 28999 28357 27730 27117 26517 25931 25358 24797 24249 23713 23188 22676 22174 21684 21205 20736))

(defparameter *rossi-vicentino-names* (list "a" "[Ȧ]" "[a♯]" "[b♭]" "[Ḃ♭]" "b" "[Ḃ]" "[b♯]" "c" "[Ċ]" "[c♯]" "[d♭]" "[Ḋ♭]" "d" "[Ḋ]" "[d♯]" "[e♭]" "[Ė♭]" "e" "[Ė]" "[e♯]" "f" "[Ḟ]" "f♯" "[g♭]" "[Ġ♭]" "g" "xg" "♯g" "♭aa" "x♭aa" "aa"))

(defparameter *rossi-sistema* (list 41472 39690 38760 37095 35499 34668 33178 32400 31008 29676 28980 27734 26542 25920 24806 24225 23184 22187 21667 20736))

(defparameter *rossi-sistema-names* (list "a" "♯a" "♭" "b" "♯b" "c" "♯c" "♭d" "d" "♯d" "♭e" "e" "♯e" "f" "♯f" "♭g" "g" "♯g" "aa" "aa"))

(defparameter *rossi-perfetto* (list 41472 39814 38880 37325 36864 35389 34560 33178 32400 31104 30720 29491 28809 27648 26542 25920 24884 24576 24300 23328 23040 22118 21600 20736))

(defparameter *rossi-perfetto-names* (list "a" "♯a" "♭" "b[alt]" "b" "♯b" "c" "♯c" "♭d" "d[alt]" "d" "♯d" "♭e" "e" "♯e" "f" "♯f" "♯f[alt]" "♭g" "g[alt]" "g" "♯g" "♭aa" "aa"))

(defun transform-string-length (string-length &optional (reference-length 41472))
  (/ 1 (/ string-length reference-length)))

(defparameter *vicentino-meantone* (list :ȧ :a♯ :b♭ :ḃ♭ :b♮ :ḃ♮ :b♯ :c :ċ :c♯ :d♭ :ḋ♭ :d :ḋ :d♯ :e♭ :ė♭ :e :ė :e♯ :f :ḟ :f♯ :g♭ :ġ♭ :g :ġ :g♯ :a♭ :ȧ♭ :a))

(let* ((btikz (make-backend-tikz :filename "lemme-rossi.tex"))
       (h-padding 13)
       (v-scaling 0.12)
       (scale-rossi-sistema
         (make-scale (mapcar #'transform-string-length *rossi-sistema*)
                     *rossi-sistema-names*
                     (pt (* 1 h-padding) 0)
                     v-scaling
                     :main-label "S. participato"))
       (scale-rossi-perfetto
         (make-scale (mapcar #'transform-string-length *rossi-perfetto*)
                     *rossi-perfetto-names*
                     (pt (* 2 h-padding) 0)
                     v-scaling
                     :main-label "S. perfetto"))
       (scale-rossi-vicentino
         (make-scale (mapcar #'transform-string-length *rossi-vicentino*)
                     *rossi-vicentino-names*
                     (pt (* 0 h-padding) 0)
                     v-scaling
                     :main-label "Vicentino"))
       (scale-vicentino-meantone
         (make-scale (cons 1/1 (mapcar (lambda (name) (vicentino-tunings:interval :tuning1
                                                                                  :a :up name))
                                       *vicentino-meantone*))
                     (cons :a *vicentino-meantone*)
                     (pt (* -2 h-padding) 0)
                     v-scaling
                     :main-label "[1/4-MT]"
                     :helper-line-endpoint (+ 2 (* 2 h-padding))))
       (scale-vicentino-31ed2
         (make-scale (cons 1/1 (mapcar (lambda (name) (vicentino-tunings:interval :31ed2
                                                                                  :a :up name))
                                       *vicentino-meantone*))
                     (cons :a *vicentino-meantone*)
                     (pt (* -1 h-padding) 0)
                     v-scaling
                     :main-label "[31ed2]")))
  (draw-with-multiple-backends (list btikz) (list scale-rossi-sistema
                                                  scale-rossi-perfetto
                                                  scale-rossi-vicentino
                                                  scale-vicentino-meantone
                                                  scale-vicentino-31ed2))
  (compile-tikz btikz))
