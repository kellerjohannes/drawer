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

(defun make-dict-entry (id note-a note-b label)
  (list id (vicentino-tunings:interval :tuning1 note-a :up note-b) label))

(defparameter *intervals*
  (list (make-dict-entry :tono :d :e "tono")
        (make-dict-entry :semitono-maggiore :e :f "semitono maggiore")
        (make-dict-entry :terza-maggiore :f :a "terza maggiore")
        (make-dict-entry :quinta :d :a "quinta")))

(defun get-interval-data (id)
  (find id *intervals* :key #'first))

(defun get-interval-ratio (id)
  (second (get-interval-data id)))

(defun get-interval-label (id)
  (third (get-interval-data id)))

(defun make-2-constellation (cons1 step1 dir1 step2 dir2 cons2)
  (let ((t1 0)
        (t2 20)
        (step-ratio1 (* (if (eq dir1 :up) 1 -1) (get-interval-ratio step1)))
        (step-ratio2 (* (if (eq dir2 :up) 1 -1) (get-interval-ratio step2)))
        (cons-ratio1 (get-interval-ratio cons1))
        (cons-ratio2 (get-interval-ratio cons2))
        (v-factor 1/25)
        (v-padding 1.3)
        (h-padding 1.6))
    (gr (list (make-voice-leading t1 t2 1/1 step-ratio2 (get-interval-label step2) v-factor
                                  (- v-padding))
              (make-voice-leading t1 t2 cons-ratio1 (* cons-ratio1 step-ratio1)
                                  (get-interval-label step2)
                                  v-factor
                                  v-padding)
              (make-consonance t1 1/1 cons-ratio1 (get-interval-label cons1) v-factor (- h-padding))
              (make-consonance t2 step-ratio2 cons-ratio2
                               (get-interval-label cons2) v-factor h-padding)))))

(defun doit ()
  (let* ((btikz (make-backend-tikz :filename "tono-ascendente-1.tex"))
         ;; (v-factor 1/25)
         ;; (t1 0)
         ;; (t2 20)
         ;; (canto (make-voice-leading t1 t2 4/3 9/8 "tono ascendente" v-factor 1.3))
         ;; (basso (make-voice-leading t1 t2 16/15 15/16 "semitono maggiore" v-factor -1.3))
         ;; (c1 (make-consonance t1 16/15 5/4 "terza maggiore" v-factor -1.3))
         ;; (c2 (make-consonance t2 1/1 3/2 "quinta" v-factor 1.6))
         (c1 (make-2-constellation :terza-maggiore :tono :up :semitono-maggiore :down :quinta))
         )
    (draw-with-multiple-backends (list btikz) (list c1))
    (compile-tikz btikz)))
