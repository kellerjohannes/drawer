(in-package :drawer)
(ql:quickload :vicentino-tunings)


(defun simplify (interval)
  (cond ((< interval 1) (simplify (* interval 2)))
        ((>= interval 2) (simplify (* interval 1/2)))
        (t interval)))


(defun make-scale (interval-list label-list origin y-scale
                   &key helper-line-endpoint
                     (main-label "")
                     (ratio-fun (lambda (interval)
                                  (vicentino-tunings:ratio->length interval))))
  (let* ((tick-length 1.3)
         (label-offset 0.5)
         (point-list (mapcar (lambda (interval)
                               (add origin (pt 0 (* y-scale (funcall ratio-fun interval)))))
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
                                 (if (consp label)
                                     (make-text (car label)
                                                (left-of (cp point (pt 0 0) (pt 0 (cdr label)))
                                                         label-offset)
                                                :h-align :right)
                                     (make-text label (left-of point label-offset)
                                                :h-align :right)))
                               label-list
                               point-list)))
         (central-line (ln origin (first (sort (copy-list point-list)
                                               #'>
                                               :key (lambda (point)
                                                      (value (y point)))))))
         (main-label (make-text main-label (below origin 4) :v-align :top))
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




(defparameter *rossi-mt* (list
                          ;; 82944 74190 69336 62016 55468 51840 46368
                          41472 38760 37095 34668 31008 27734 25920 23184 20736))

(defparameter *rossi-mt-names* (list
                                ;; "A" "♮" "C" "D" "E" "F" "G"
                                "a" "b" "♮" "c" "d" "e" "f" "g" "aa"))

(defparameter *rossi-vicentino* (list 41472 40554 39657 38781 37923 37084 36264 35463 34679 33912 33162 32429 31712 31011 30325 29655 28999 28357 27730 27117 26517 25931 25358 24797 24249 23713 23188 22676 22174 21684 21205 20736))

(defparameter *rossi-vicentino-names* (list "a" "[Ȧ]" "[♯a]" "[♭b]" "[♭Ḃ]" "b" "[Ḃ]" "[♯b]" "c" "[Ċ]" "[♯c]" "[♭d]" "[♭Ḋ]" "d" "[Ḋ]" "[♯d]" "[♭e]" "[♭Ė]" "e" "[Ė]" "[♯e]" "f" "[Ḟ]" "♯f" "[♭g]" "[♭Ġ]" "g" "×g" "♯g" "♭aa" "×♭aa" "aa"))

(defparameter *rossi-sistema* (list 41472 39690 38760 37095 35499 34668 33178 32400 31008 29676 28980 27734 26542 25920 24806 24225 23184 22187 21667 20736))

(defparameter *rossi-sistema-names* (list "a" "♯a" "♭" "♮" "♯b" "c" "♯c" "♭d" "d" "♯d" "♭e" "e" "♯e" "f" "♯f" "♭g" "g" "♯g" "♭aa" "aa"))

(defparameter *rossi-perfetto* (list 41472 39814 38880 37325 36864 35389 34560 33178 32400 31104 30720 29491 28809 27648 26542 25920 24884 24576 24300 23328 23040 22118 21600 20736))

(defparameter *rossi-perfetto-names* (list "a" "♯a" "♭" "♮[alt]" "♮" "♯b" "c" "♯c" "♭d" "d[alt]" "d" "♯d" "♭e" "e" "♯e" "f" "♯f" "♯f[alt]" "♭g" "g[alt]" "g" "♯g" "♭aa" "aa"))

(defun transform-string-length (string-length &optional (reference-length 41472))
  (/ 1 (/ string-length reference-length)))

(defparameter *vicentino-meantone* (list :ȧ :a♯ :b♭ :ḃ♭ :b♮ :ḃ♮ :b♯ :c :ċ :c♯ :d♭ :ḋ♭ :d :ḋ :d♯ :e♭ :ė♭ :e :ė :e♯ :f :ḟ :f♯ :g♭ :ġ♭ :g :ġ :g♯ :a♭ :ȧ♭ :a))

(let* ((btikz (make-backend-tikz :filename "lemme-rossi.tex"))
       (h-padding 13)
       (v-scaling 0.12)
       (scale-vicentino-meantone
         (make-scale (cons 1/1 (mapcar (lambda (name) (vicentino-tunings:interval :tuning1
                                                                                  :a :up name))
                                       *vicentino-meantone*))
                     (cons :a *vicentino-meantone*)
                     (pt (* -3 h-padding) 0)
                     v-scaling
                     :main-label "[1/4-MT]"
                     :helper-line-endpoint (+ 2 (* 2 h-padding))))
       (scale-rossi-mt
         (make-scale (mapcar #'transform-string-length *rossi-mt*)
                     *rossi-mt-names*
                     (pt (* -2 h-padding) 0)
                     v-scaling
                     :main-label "1/4-MT Rossi"))
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
       (scale-vicentino-31ed2
         (make-scale (cons 1/1 (mapcar (lambda (name) (vicentino-tunings:interval :31ed2
                                                                                  :a :up name))
                                       *vicentino-meantone*))
                     (cons :a *vicentino-meantone*)
                     (pt (* -1 h-padding) 0)
                     v-scaling
                     :main-label "[31ed2]")))
  (draw-with-multiple-backends (list btikz) (list scale-rossi-mt
                                                  scale-rossi-sistema
                                                  scale-rossi-perfetto
                                                  scale-rossi-vicentino
                                                  scale-vicentino-meantone
                                                  scale-vicentino-31ed2))
  (compile-tikz btikz))


(defparameter *vicentino-meantone-with-sesto* (list :f’ :ḟ :f♯ :g♭ :ġ♭ :g :g’ :ġ :g♯ :a♭ :ȧ♭ :a :aʼ :ȧ :a♯ :b♭ :ḃ♭ :b♮ :B♮ʼ :ḃ♮ :b♯ :c :c’ :ċ :c♯ :d♭ :ḋ♭ :d :d’ :ḋ :d♯ :e♭ :ė♭ :e :e’ :ė :e♯ :f ))

(defparameter *vicentino-meantone-with-sesto-names* (list :f nil :ḟ :f♯ :g♭ :ġ♭ :g nil :ġ :g♯ :a♭ :ȧ♭ :a nil :ȧ :a♯ :b♭ :ḃ♭ :b♮ nil :ḃ♮ :b♯ :c nil :ċ :c♯ :d♭ :ḋ♭ :d nil :ḋ :d♯ :e♭ :ė♭ :e nil :ė :e♯ :f ))

(defparameter *maniates* (reverse (list 20736 21205 21684 21928 22174 22676 23188 23713 24249 24521 24797 25358 25931 26517 27117 27421 27730 28357 28999 29325 29655 30325 31011 31712 32429 32793 33162 33912 34679 35463 36264 36672 37084 37923 38781 39657 40554 41012 41472)))

(defparameter *maniates-names* (list "1F" "[(6Ƒ)]" "4Ḟ" "2F♯" "3G♭" "5Ġ♭" "1G" "[6Ɠ]" "4Ġ" "2G♯" "3A♭" "5Ȧ♭" "1A" "[6Ả]" "4Ȧ" "3A♯" "2B♭" "5Ḃ♭" "1B" "[6Ɓ]" "4Ḃ" "3B♯" "1C" "[(6Ƈ)]" "4Ċ" "2C♯" "3D♭" "5Ḋ♭" "1D" "[6Ɗ]" "4Ḋ" "3D♯" "2E♭" "5Ė♭" "1E" "[6Ẻ]" "4Ė♭" "3E♯" "1F"))


(let* ((btikz (make-backend-tikz :filename "lemme-rossi-maniates.tex"))
       (h-padding 13)
       (v-scaling 0.15)
       (scale-maniates
         (make-scale (mapcar #'transform-string-length *maniates*)
                     *maniates-names*
                     (pt (* -3 h-padding) 0)
                     v-scaling
                     :main-label "Maniates"
                     :helper-line-endpoint (+ 2 (* 0 h-padding))))
       (scale-vicentino-meantone
         (make-scale (cons 1/1 (mapcar (lambda (name) (vicentino-tunings:interval :tuning1
                                                                                  :f :up name))
                                       *vicentino-meantone-with-sesto*))
                     *vicentino-meantone-with-sesto-names*
                     (pt (* 0 h-padding) 0)
                     v-scaling
                     :main-label "[1/4-MT]"
                     ))

       (scale-vicentino-31ed2
         (make-scale (cons 1/1 (mapcar (lambda (name) (vicentino-tunings:interval :31ed2
                                                                                  :f :up name))
                                       *vicentino-meantone*))
                     (cons :f *vicentino-meantone*)
                     (pt (* -1 h-padding) 0)
                     v-scaling
                     :main-label "[31ed2]"))
       (scale-rossi-vicentino
         (make-scale (mapcar #'transform-string-length *rossi-vicentino*)
                     *rossi-vicentino-names*
                     (pt (* -2 h-padding) 0)
                     v-scaling
                     :main-label "Rossi"))
       )
  (draw-with-multiple-backends (list btikz) (list scale-maniates
                                                  scale-vicentino-meantone
                                                  scale-vicentino-31ed2
                                                  scale-rossi-vicentino))
  (compile-tikz btikz))


(defparameter *maniates-secondo* (list 1 5 78 83 116 160 194 199 276 271 310 315 387 392 464 470 503 508 581 586 658 663 697 702 774 780 813 818 890 896 968 973 1006 1012 1084 1089 1161 1166 1200))

(defparameter *maniates-secondo-names* '(("1F" . -0.9) ("4Ḟ" . 0.9)
                                         ("2F♯" . -0.9) ("5Ġ♭" . 0.9)
                                         "3G♭" "6Ɠ"
                                         ("1G" . -0.9) ("4Ġ" . 0.9)
                                         ("5Ȧ♭" . -1.9) ("2G♯" . 0.9)
                                         ("3A♭" . -0.9) ("6Ả" . 0.9)
                                         ("1A" . -0.9) ("4Ȧ" . 0.9)
                                         ("3A♯" . -0.9) ("6Ɓ" . 0.9)
                                         ("2B♭" . -0.9) ("5Ḃ♭" . 0.9)
                                         ("1B" . -0.9) ("4Ḃ" . 0.9)
                                         ("3B♯" . -0.9) ("[6Ƈ]" . 0.9)
                                         ("1C" . -0.9) ("4Ċ" . 0.9)
                                         ("2C♯" . -0.9) ("5Ḋ♭" . 0.9)
                                         ("3D♯" . -0.9) ("6Ɗ" . 0.9)
                                         ("1D" . -0.9) ("4Ḋ" . 0.9)
                                         ("3D♯" . -0.9) ("5Ė♭" . 0.9)
                                         ("2E♭" . -0.9) ("6Ẻ" . 0.9)
                                         ("1E" . -0.9) ("4Ė" . 0.9)
                                         ("3E♯" . -0.9) ("[6Ƒ]" . 0.9)
                                         "1F"))

(defparameter *vicentino-meantone-secondo* (list :f’ :f♯ :f♯’ :g♭ :g♭’ :g :g’ :g♯ :g♯’ :a♭ :a♭’ :a :a’ :a♯ :a♯’ :b♭ :b♭’ :b♮ :b♮ʼ :b♯ :b♯’ :c :c’ :c♯ :c♯’ :d♭ :d♭’ :d :d’ :d♯ :d♯’ :e♭ :e♭’ :e :e’ :e♯ :e♯’ :f ))

(defparameter *vicentino-meantone-secondo-names* '(("F [1F]" . -0.9) ("F’ [4Ḟ]" . 0.9)
                                                  ("F♯ [2F♯]" . -0.9) ("F♯’ [5Ġ♭]" . 0.9)
                                                  ("G♭ [3G♭]" . -0.9) ("G♭’ [6Ɠ]" . 0.9)
                                                  ("G [1G]" . -0.9) ("G’ [4Ġ]" . 0.9)
                                                  ("G♯ [2G♯]" . -0.9) ("G♯’ [5Ȧ♭]" . 0.9)
                                                  ("A♭ [3A♭]" . -0.9) ("A♭’ [6Ả]" . 0.9)
                                                  ("A [1A]" . -0.9) ("A’ [4Ḟ]" . 0.9)
                                                  ("A♯ [3A♯]" . -0.9) ("A♯’ [6Ɓ]" . 0.9)
                                                  ("B♭ [2B♭]" . -0.9) ("B♭’ [5Ḃ♭]" . 0.9)
                                                  ("B♮ [1B]" . -0.9) ("B♮ʼ [4Ḃ]" . 0.9)
                                                  ("B♯ [3B♯]" . -0.9) ("B♯’ [6Ƈ]" . 0.9)
                                                  ("C [1C]" . -0.9) ("C’ [4Ċ]" . 0.9)
                                                  ("C♯ [2C♯]" . -0.9) ("C♯’ [5Ḋ♭]" . 0.9)
                                                  ("D♭ [3D♭]" . -0.9) ("D♭’ [6Ɗ]" . 0.9)
                                                  ("D [1D]" . -0.9) ("D’ [4Ḋ]" . 0.9)
                                                  ("D♯ [3D♯]" . -0.9) ("D♯’ [6Ẻ]" . 0.9)
                                                  ("E♭ [2E♭]" . -0.9) ("E♭’ [5Ė♭]" . 0.9)
                                                  ("E [1E]" . -0.9) ("E’ [4Ė]" . 0.9)
                                                  ("E♯ [3E♯]" . -0.9) ("E♯’ [6Ƒ]" . 0.9)
                                                  "F [F1]"))

(let* ((btikz (make-backend-tikz :filename "maniates-second-tuning.tex"))
       (h-padding 13)
       (v-scaling 0.15)
       (scale-maniates
         (make-scale *maniates-secondo*
                     *maniates-secondo-names*
                     (pt (* -1 h-padding) 0)
                     v-scaling
                     :ratio-fun (lambda (interval-cents) interval-cents)
                     :main-label "Maniates"
                     :helper-line-endpoint (+ 2 (* 1 h-padding))))
       (scale-vicentino-meantone
         (make-scale (cons 1/1 (mapcar (lambda (name) (vicentino-tunings:interval :tuning1
                                                                                  :f :up name))
                                       *vicentino-meantone-secondo*))
                     *vicentino-meantone-secondo-names*
                     (pt (* 0 h-padding) 0)
                     v-scaling
                     :main-label "[1/4-MT]"
                     ))
       (scale-vicentino-31ed2
         (make-scale (cons 1/1 (mapcar (lambda (name) (vicentino-tunings:interval :31ed2
                                                                                  :f :up name))
                                       *vicentino-meantone*))
                     (cons :f *vicentino-meantone*)
                     (pt (* 1 h-padding) 0)
                     v-scaling
                     :main-label "[31ed2]"))
       )
  (draw-with-multiple-backends (list btikz) (list scale-maniates
                                                  scale-vicentino-meantone
                                                  scale-vicentino-31ed2))
  (compile-tikz btikz))
