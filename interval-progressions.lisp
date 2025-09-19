(in-package :drawer)

(defun rad->deg (rad)
  (/ (* rad 180.0) PI))

(defun make-voice-leading (x1 x2 ratio1 ratio-delta label y-scale label-padding)
  (let ((a (pt x1 (* y-scale (vicentino-tunings:ratio->length ratio1))))
        (h (- (* y-scale (vicentino-tunings:ratio->length (* ratio1 ratio-delta)))
              (* y-scale (vicentino-tunings:ratio->length ratio1))))
        (b (pt x2 (* y-scale (vicentino-tunings:ratio->length (* ratio1 ratio-delta))))))
    (gr (list (ln a b :style-update '(:line-type :thick))
              (make-text label
                         (cp (midpoint a b)
                             (pt 0 0)
                             (let* ((x (- h))
                                    (y (- x2 x1))
                                    (len (sqrt (+ (* x x) (* y y)))))
                               (pt (* (/ x len) label-padding)
                                   (* (/ y len) label-padding))))
                         :angle (rad->deg (atan (/ h (- x2 x1)))))))))

(defun make-consonance (x ratio1 ratio2 label y-scale label-padding)
  (let ((a (pt x (* y-scale (vicentino-tunings:ratio->length ratio1))))
        (b (pt x (* y-scale (vicentino-tunings:ratio->length (* ratio1 ratio2))))))
    (gr (list (ln a b :style-update '(:line-type :dotted))
              (make-text label
                         (cp (midpoint a b)
                             (pt 0 0)
                             (pt label-padding 0))
                         :angle 90)))))

(defun make-dict-entry (id note-a note-b label &optional (octave 0))
  (list id
        (* (vicentino-tunings:interval :tuning1 note-a :up note-b)
           (expt 2 octave))
        label))

(defparameter *intervals*
  (list
   (make-dict-entry :unisono :d :d "unisono" -1)
   (make-dict-entry :semitono-maggiore :e :f "semitono maggiore")
   (make-dict-entry :tono :d :e "tono")
   (make-dict-entry :terza-minore :d :f "terza minore")
   (make-dict-entry :terza-maggiore :f :a "terza maggiore")
   (make-dict-entry :quarta :d :g "quarta")
   (make-dict-entry :quinta :d :a "quinta")
   (make-dict-entry :ottava :d :d "ottava")
   (make-dict-entry :decima :c :e "decima" 1)))

(defun get-interval-data (id)
  (find id *intervals* :key #'first))

(defun get-interval-ratio (id)
  (second (get-interval-data id)))

(defun get-interval-label (id)
  (third (get-interval-data id)))


(defun invert-ratio (ratio)
  (/ 1 ratio))

(defun make-2-constellation (cons1 step1 dir1 step2 dir2 cons2 &optional (v-transpose :unisono) (transpose-dir :up))
  (let ((t1 0)
        (t2 20)
        (step-ratio1 (* (if (eq dir1 :up)
                            (get-interval-ratio step1)
                            (invert-ratio (get-interval-ratio step1)))))
        (step-ratio2 (* (if (eq dir2 :up)
                            (get-interval-ratio step2)
                            (invert-ratio (get-interval-ratio step2)))))
        (cons-ratio1 (get-interval-ratio cons1))
        (cons-ratio2 (get-interval-ratio cons2))
        (v-factor 1/25)
        (v-padding 1.3)
        (h-padding 1.6))
    (cp (gr (list (make-voice-leading t1 t2 1/1 step-ratio2 (get-interval-label step2)
                                      v-factor (- v-padding))
                  (make-voice-leading t1 t2 cons-ratio1 step-ratio1 (get-interval-label step1)
                                      v-factor
                                      v-padding)
                  (make-consonance t1 1/1 cons-ratio1 (get-interval-label cons1) v-factor (- h-padding))
                  (make-consonance t2 step-ratio2 cons-ratio2
                                   (get-interval-label cons2) v-factor h-padding)))
        (pt 0 0)
        (pt 0 (* (if (eq transpose-dir :up) 1 -1)
                 (* v-factor (vicentino-tunings:ratio->length (get-interval-ratio v-transpose))))))))

(defun inverse-direction (dir)
  (if (eq dir :up) :down :up))

(defun make-constellation-group (cons1 step1 dir1 step2 dir2 cons2 h-padding)
  (let ((ca (make-2-constellation cons1 step1 dir1 step2 dir2 cons2))
        (cb (make-2-constellation cons2 step1 (inverse-direction dir1)
                                  step2 (inverse-direction dir2) cons1
                                  step2 dir2))
        (cc (make-2-constellation cons1 step2 (inverse-direction dir2) step1 (inverse-direction dir1) cons2))
        (cd (make-2-constellation cons2 step2 dir2 step1 dir1 cons1
                                  step1 (inverse-direction dir1))))
    (if (eq step1 step2)
        (gr (list ca
                  (cp cb (pt 0 0) (pt h-padding 0))
                  ;; (cp cc (pt 0 0) (pt (* 2 h-padding) 0))
                  ;; (cp cd (pt 0 0) (pt (* 3 h-padding) 0))
                  ))
        (gr (list ca
                  (cp cb (pt 0 0) (pt h-padding 0))
                  (cp cc (pt 0 0) (pt (* 2 h-padding) 0))
                  (cp cd (pt 0 0) (pt (* 3 h-padding) 0)))))))

(let* ((btikz (make-backend-tikz :filename "tono-ascendente-1.tex"))
       (h-padding 35)
       (v-padding 65)
       (cg1 (make-constellation-group :terza-minore :tono :up :quinta :down :ottava
                                      h-padding))
       (cg2 (make-constellation-group :terza-maggiore :tono :up :semitono-maggiore :down :quinta
                                      h-padding))
       (cg3 (make-constellation-group :terza-minore :tono :up :tono :down :quinta
                                  h-padding))
       (cg4 (make-constellation-group :terza-minore :tono :up :quinta :down :ottava
                                  h-padding))
       )
  (draw-with-multiple-backends (list btikz) (list
                                             cg1
                                             (cp cg2 (pt 0 0) (pt 0 (* -1 v-padding)))
                                             (cp cg3 (pt 0 0) (pt 0 (* -2 v-padding)))
                                             (cp cg4 (pt 0 0) (pt 0 (* -3 v-padding)))
                                             ))
  (compile-tikz btikz))
