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


(defparameter *maniates-secondo* (list 0 5 78 83 116 160 194 199 276 271 310 315 387 392 464 470 503 508 581 586 658 663 697 702 774 780 813 818 890 896 968 973 1006 1012 1084 1089 1161 1166 1200))

(defparameter *maniates-secondo-names* '(("1F [F]" . -0.9) ("4Ḟ [F’]" . 0.9)
                                         ("2F♯ [F♯]" . -0.9) ("5Ġ♭ [F♯’]" . 0.9)
                                         "3G♭ [G♭]" "6Ɠ [F♯♯’]"
                                         ("1G [G]" . -0.9) ("4Ġ [G’]" . 0.9)
                                         ("5Ȧ♭ [G♯’]" . 0.9) ("2G♯ [G♯]" . -0.9)
                                         ("3A♭ [A♭]" . -0.9) ("6Ả [A♭’]" . 0.9)
                                         ("1A [A]" . -0.9) ("4Ȧ [A’]" . 0.9)
                                         ("3A♯ [A♯]" . -0.9) ("6Ɓ [A♯’]" . 0.9)
                                         ("2B♭ [B♭]" . -0.9) ("5Ḃ♭ [B♭’]" . 0.9)
                                         ("1B [B♮]" . -0.9) ("4Ḃ [B♮’]" . 0.9)
                                         ("3B♯ [B♯]" . -0.9) ("(6Ƈ) [B♯’]" . 0.9)
                                         ("1C [C]" . -0.9) ("4Ċ [C’]" . 0.9)
                                         ("2C♯ [C♯]" . -0.9) ("5Ḋ♭ [C♯’]" . 0.9)
                                         ("3D♯! [D♭]" . -0.9) ("6Ɗ [D♭’]" . 0.9)
                                         ("1D [D]" . -0.9) ("4Ḋ [D’]" . 0.9)
                                         ("3D♯ [D♯]" . -0.9) ("5Ė♭ [D♯’]" . 0.9)
                                         ("2E♭ [E♭]" . -0.9) ("6Ẻ [E♭’]" . 0.9)
                                         ("1E [E]" . -0.9) ("4Ė [E’]" . 0.9)
                                         ("3E♯ [E♯]" . -0.9) ("(6Ƒ) [E♯’]" . 0.9)
                                         "1F"))


(defparameter *maniates-secondo-1975* (mapcar (lambda (c) (let ((transposition (- c 503.4)))
                                                            (if (< transposition 0)
                                                                (+ transposition 1200)
                                                                transposition)) )
                                              (list 503.4 508.8 579.5 620.5 584.9 696.6 702 772.6 778.1 813.6 819.1 889.7 895.2 965.8 971.3 1006.8 1012.3 1082.9 1088.3 1159 1164.4 1200 1205.4 76.1 117.1 122.5 193.2 198.6 269.3 274.6 310.3 315.6 386.3 391.6 462.4 467.8 (+ 1200 503.4))))

(defparameter *maniates-secondo-1975-names* '(("1f [F]" . -0.9) ("4f [F’]" . 0.9)
                                              ("2f♯ [F♯]" . -0.9)
                                              ("3g♭ [G♭]" . 0.0)
                                              ("5g♭ [F♯’]" . 0.9)
                                              ("1g [G]" . -0.9) ("4g [G’]" . 0.9)
                                              ("2g♯ [G♯]" . -0.9) ("6g♯ [G♯’]" . 0.9)
                                              ("3a♭ [A♭]" . -0.9) ("5a♭ [A♭’]" . 0.9)
                                              ("1a [A]" . -0.9) ("4a [A’]" . 0.9)
                                              ("3a♯ [A♯]" . -0.9) ("6a♯ [A♯’]" . 0.9)
                                              ("2b♭ [B♭]" . -0.9) ("5b♭ [B♭’]" . 0.9)
                                              ("1b [B♮]" . -0.9) ("4b [B♮’]" . 0.9)
                                              ("3b♯ [B♯]" . -0.9) ("6b♯ [B♯’]" . 0.9)
                                              ("1c [C]" . -0.9) ("4c [C’]" . 0.9)
                                              ("2c♯ [C♯]" . -0.0)
                                              ("3d♭ [D♭]" . -0.9) ("5d♭ [D♭’]" . 0.9)
                                              ("1d [D]" . -0.9) ("4d [D’]" . 0.9)
                                              ("3d♯ [D♯]" . -0.9) ("6d♯ [D♯’]" . 0.9)
                                              ("2e♭ [E♭]" . -0.9) ("5e♭ [E♭’]" . 0.9)
                                              ("1e [E]" . -0.9) ("4e [E’]" . 0.9)
                                              ("3e♯ [E♯]" . -0.9) ("6e♯ [E♯’]" . 0.9)
                                              "1f [F]"))

(defparameter *vicentino-meantone-secondo* (list :f’ :f♯ :f♯’ :g♭ :g♭’ :g :g’ :g♯ :g♯’ :a♭ :a♭’ :a :a’ :a♯ :a♯’ :b♭ :b♭’ :b♮ :b♮ʼ :b♯ :b♯’ :c :c’ :c♯ :c♯’ :d♭ :d♭’ :d :d’ :d♯ :d♯’ :e♭ :e♭’ :e :e’ :e♯ :e♯’ :f ))

(defparameter *vicentino-meantone-secondo-names* '(("F" . -0.9) ("F’" . 0.9)
                                                  ("F♯" . -0.9) ("F♯’" . 0.9)
                                                  ("G♭" . -0.9) ("G♭’" . 0.9)
                                                  ("G" . -0.9) ("G’" . 0.9)
                                                  ("G♯" . -0.9) ("G♯’" . 0.9)
                                                  ("A♭" . -0.9) ("A♭’" . 0.9)
                                                  ("A" . -0.9) ("A’" . 0.9)
                                                  ("A♯" . -0.9) ("A♯’" . 0.9)
                                                  ("B♭" . -0.9) ("B♭’" . 0.9)
                                                  ("B♮" . -0.9) ("B♮ʼ" . 0.9)
                                                  ("B♯" . -0.9) ("B♯’" . 0.9)
                                                  ("C" . -0.9) ("C’" . 0.9)
                                                  ("C♯" . -0.9) ("C♯’" . 0.9)
                                                  ("D♭" . -0.9) ("D♭’" . 0.9)
                                                  ("D" . -0.9) ("D’" . 0.9)
                                                  ("D♯" . -0.9) ("D♯’" . 0.9)
                                                  ("E♭" . -0.9) ("E♭’" . 0.9)
                                                  ("E" . -0.9) ("E’" . 0.9)
                                                  ("E♯" . -0.9) ("E♯’" . 0.9)
                                                  "F"))

(let* ((btikz (make-backend-tikz :filename "maniates-second-tuning.tex"))
       (h-padding 13)
       (v-scaling 0.15)
       (scale-maniates
         (make-scale *maniates-secondo*
                     *maniates-secondo-names*
                     (pt (* -2 h-padding) 0)
                     v-scaling
                     :ratio-fun (lambda (interval-cents) interval-cents)
                     :main-label "Maniates 1996"
                     :helper-line-endpoint (+ 2 (* 1 h-padding))))
       (scale-vicentino-31ed2
         (make-scale (cons 1/1 (mapcar (lambda (name) (vicentino-tunings:interval :31ed2
                                                                                  :f :up name))
                                       *vicentino-meantone*))
                     (cons :f *vicentino-meantone*)
                     (pt (* -1 h-padding) 0)
                     v-scaling
                     :main-label "[31ed2]"))
       (scale-vicentino-meantone
         (make-scale (cons 1/1 (mapcar (lambda (name) (vicentino-tunings:interval :tuning1
                                                                                  :f :up name))
                                       *vicentino-meantone-secondo*))
                     *vicentino-meantone-secondo-names*
                     (pt (* 0 h-padding) 0)
                     v-scaling
                     :main-label "[1/4-MT]"
                     ))
       (scale-maniates-1975
         (make-scale *maniates-secondo-1975*
                     *maniates-secondo-1975-names*
                     (pt (* 1 h-padding) 0)
                     v-scaling
                     :ratio-fun (lambda (interval-cents) interval-cents)
                     :main-label "Maniates 1975"))
       )
  (draw-with-multiple-backends (list btikz) (list scale-maniates
                                                  scale-maniates-1975
                                                  scale-vicentino-meantone
                                                  scale-vicentino-31ed2))
  (compile-tikz btikz))
