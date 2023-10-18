(in-package :drawer)


(defclass circle-of-fifths (tonsystem)
  ((label-array :initarg :label-array :accessor label-array)
   (tick-array :initarg :tick-array :accessor tick-array)
   (offset :initarg :offset :accessor offset)
   (circle-line :initarg :circle-line :accessor circle-line)
   (connection-list :initarg :connection-list :accessor connection-list)))

(defun make-circle-of-fifths (circle-line ticks labels offset)
  (make-instance 'circle-of-fifths :tick-array ticks :label-array labels :offset offset
                 :circle-line circle-line))

(defmethod add-connection ((cof-a circle-of-fiths) (cof-b circle-of-fifths)
                           position-a position-b &key (style-update nil))
  )

(defmethod add-connections ((cof-a circle-of-fifths) (cof-b circle-of-fifths) delta start
                            &key (style-update nil))

  )

(defmethod make-circle-position ((center point) radius start-angle end-angle num index)
  (cp (rotate-point (pt radius 0)
                    (+ start-angle (* index (/ (- end-angle start-angle) (1- num)))))
      (pt 0 0)
      center))

(defmethod generate-ticks ((center point) radius tick num start-angle end-angle)
  (let ((result (make-array num)))
    (loop for i from 0 to (1- num)
          do (setf (aref result i)
                   (cp tick
                       (pt 0 0)
                       (make-circle-position center radius start-angle end-angle num i))))
    result))

(defmethod generate-labels ((center point) radius label-list start-angle end-angle air)
  (let ((result (make-array (length label-list))))
    (loop for i from 0 to (1- (length label-list))
          for label in label-list
          do (setf (aref result i)
                   (make-text label (move-point-towards
                                     (make-circle-position center radius start-angle end-angle
                                                           (length label-list) i)
                                     center
                                     air))))
    result))

(defmethod cof ((center point) radius start-angle end-angle tick-object lbl-list lbl-offset id-offset)
  "`tick-object' needs to be relative to (0,0)."
  (make-circle-of-fifths (arc center radius start-angle end-angle)
                         (generate-ticks center radius tick-object (length lbl-list) start-angle end-angle)
                         (generate-labels center radius lbl-list start-angle end-angle lbl-offset)
                         id-offset))




(let* ((btikz (make-backend-tikz :filename "circle-adaptive-just-quartercomma.tex"))
       (tick (circ 0 0 .4 :style-update '(:fill :fill)))
       (qz-lower (cof (pt 50 50) 40 180 0 tick
                      '("G♭" "D♭" "A♭" "E♭" "B♭" "F" "C" "G" "D" "A" "E" "B♮" "F♯" "C♯" "G♯" "D♯" "A♯" "E♯" "B♯") -2.5 7))
       (qz-upper (cof (pt 50 50) 30 180 0 tick
                      '("G♭" "D♭" "A♭" "E♭" "B♭" "F" "C" "G" "D" "A" "E" "B♮" "F♯" "C♯" "G♯" "D♯" "A♯") 2.5 7)))
  (draw-with-multiple-backends (list btikz) (list qz-lower qz-upper))
  (compile-tikz btikz))



(let* ((btikz (make-backend-tikz :filename "circle-adaptive-just-thirdcomma.tex"))
       (tick (circ 0 0 .4 :style-update '(:fill :fill)))
       (qz-lower (cof (pt 50 50) 40 220 -140 tick
                      '("G♭" "D♭" "A♭" "E♭" "B♭" "F" "C" "G" "D" "A" "E" "B♮" "F♯" "C♯" "G♯" "D♯" "A♯" "E♯" "B♯" nil) -2.5 7))
       (qz-upper (cof (pt 50 50) 30 220 -140 tick
                      '("G♭" "D♭" "A♭" "E♭" "B♭" "F" "C" "G" "D" "A" "E" "B♮" "F♯" "C♯" "G♯" "D♯" "A♯" nil nil nil) 2.5 7)))
  (draw-with-multiple-backends (list btikz) (list qz-lower qz-upper))
  (compile-tikz btikz))
