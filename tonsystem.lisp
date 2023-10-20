(in-package :drawer)

(defclass tonsystem ()
  ())

(defclass tonnetz-node ()
  ((label :initarg :label :initform nil :accessor label)))

(defmethod print-object ((object tonnetz-node) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "label: ~a" (label object))))



(defclass tonnetz (tonsystem)
  ((dimensions :initform '(1 1) :initarg :dimensions :accessor dimensions)
   (offsets :initform '(0 0) :initarg :offsets :accessor offsets)
   (nodes :accessor nodes)
   (connection-list :initform nil :accessor connection-list)))

(defmethod initialize-instance :after ((system tonnetz) &key)
  (setf (nodes system) (make-array (dimensions system)))
  (loop for i from 0 to (1- (reduce #'* (dimensions system)))
        do (setf (row-major-aref (nodes system) i) (make-instance 'tonnetz-node))))

(defmethod print-netz ((system tonnetz))
  (format t "~&~a" (nodes system)))

(defmethod make-tonnetz (dimension-list offset-list)
  (make-instance 'tonnetz :dimensions dimension-list :offsets offset-list))

(defun construct-subscripts (dimension-mask index)
  (let ((result (copy-list dimension-mask)))
    (setf (elt result (position nil result)) index)
    result))

(defmethod get-offset-subscripts ((system tonnetz) subscripts)
  "Takes human readable subscripts, turns them into 0-based subscripts."
  (if (= (length subscripts) (length (offsets system)))
      (mapcar #'+ subscripts (offsets system))
      (error "Provided subscripts ~a don't match the offsets of the system, ~a."
             subscripts (dimensions system))))

(defmethod make-offset-subscripts ((system tonnetz) subscripts)
  "Takes 0-based subscripts, turns them into human readable subscripts."
  (mapcar #'- subscripts (offsets system)))

(defmethod get-node ((system tonnetz) subscripts)
  (apply #'aref (nodes system) (get-offset-subscripts system subscripts)))

(defmethod get-node-0 ((system tonnetz) subscripts)
  (apply #'aref (nodes system) subscripts))

(defmethod init-row ((system tonnetz) dimension-mask start-position label-list)
  (loop for lbl in label-list
        for i from start-position
        do (setf (label (get-node system (construct-subscripts dimension-mask i)))
                 lbl)))

(defun connection-hit-p (location-pair connection)
  (format t "~&~s" location-pair)
  (or (and (equal (first location-pair) (getf connection :a))
           (equal (second location-pair) (getf connection :b)))
      (and (equal (first location-pair) (getf connection :b))
           (equal (second location-pair) (getf connection :a)))))

(defmethod find-connection ((system tonnetz) location-a location-b)
  (find (list location-a location-b) (connection-list system) :test #'connection-hit-p))

(defparameter *testnetz* (make-tonnetz '(5 4 3) '(2 2 1)))

(defmethod in-range ((system tonnetz) location)
  (loop for element in (mapcar (lambda (coordinate dimension offset)
                                 (and (>= coordinate (- offset))
                                      (< coordinate (+ (- offset) dimension))))
                               location
                               (dimensions system)
                               (offsets system))
        always element))

(defmethod add-connection ((system tonnetz) location-a location-b &key (style-update nil))
  (unless (find-connection system location-a location-b)
    (push (list :a location-a :b location-b :style (update-style *default-style* style-update))
          (connection-list system))))

(defmethod add-connections ((system tonnetz) path &key (style-update nil))
  (loop for major-index from 0 to (1- (array-total-size (nodes system)))
        do (let* ((origin (make-offset-subscripts system
                                                  (alexandria-2:rmajor-to-indices
                                                   (dimensions system) major-index)))
                  (target (mapcar #'+ origin path)))
             (when (and (in-range system target)
                        (label (get-node system origin))
                        (label (get-node system target)))
               (add-connection system origin target :style-update style-update)))))

(defmethod remove-connection ((system tonnetz) location-a location-b)
  (when (find-connection system location-a location-b)
    (setf (connection-list system) (remove-if (lambda (connection)
                                                (connection-hit-p (list location-a location-b)
                                                                  connection))
                                              (connection-list system)))))

(defmethod update-connection-style ((system tonnetz) location-a location-b update)
  (let ((connection (find-connection system location-a location-b)))
    (when connection
      (remove-connection system location-a location-b)
      (add-connection system location-a location-b
                      :style-update (update-style (getf connection :style) update)))))






(defclass circle-of-fifths (tonsystem)
  ((label-array :initarg :label-array :accessor label-array)
   (tick-array :initarg :tick-array :accessor tick-array)
   (position-array :initarg :position-array :accessor position-array)
   (offset :initarg :offset :accessor offset)
   (circle-line :initarg :circle-line :accessor circle-line)
   (connection-list :initform nil :accessor connection-list)))

(defun make-circle-of-fifths (circle-line positions ticks labels offset)
  (make-instance 'circle-of-fifths :position-array positions
                                   :tick-array ticks
                                   :label-array labels
                                   :offset offset
                                   :circle-line circle-line))

(defmethod extract-circle-position ((cof circle-of-fifths) index)
  (aref (position-array cof) (+ index (offset cof))))

(defmethod index-in-range-p ((cof circle-of-fifths) index)
  (and (>= index (- (offset cof)))
       (< index (- (length (position-array cof)) (offset cof)))))

(defmethod add-circle-connection ((cof-a circle-of-fifths) index-a index-b
                                  &key (cof-b cof-a) (style-update nil))
  "Make a line between ticks of the circle. Optionally a second `circle-of-fifths' can be provided to
draw connections between two circles. The connections are stored in `cof-a'."
  (when (and (index-in-range-p cof-a index-a)
             (index-in-range-p cof-b index-b))
    (push (make-line (extract-circle-position cof-a index-a)
                     (extract-circle-position cof-b index-b)
                     :style (if style-update (update-style *default-style* style-update)
                                *default-style*))
          (connection-list cof-a))))

(defmethod add-circle-connections ((cof-a circle-of-fifths) delta start
                                   &key (cof-b cof-a) (style-update nil)
                                     (stop (- (length (position-array cof-a)) (offset cof-a))))
  (loop for i from start to stop
        do (add-circle-connection cof-a i (+ i delta) :cof-b cof-b :style-update style-update)))

(defmethod make-circle-position ((center point) radius start-angle end-angle num index)
  (cp (rotate-point (pt radius 0)
                    (+ start-angle (* index (/ (- end-angle start-angle) (1- num)))))
      (pt 0 0)
      center))


(defmethod generate-positions ((center point) radius num start-angle end-angle)
  (let ((result (make-array num)))
    (dotimes (i num)
      (setf (aref result i) (make-circle-position center radius start-angle end-angle num i)))
    result))


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



(defmethod generate-text-labels ((system tonnetz) (zero-point point) axis-delta-list)
  (let ((result nil))
    (loop for major-index from 0 to (1- (array-total-size (nodes system)))
          do (let ((subscripts (alexandria-2:rmajor-to-indices (dimensions system) major-index)))
               (push (make-text (label (get-node-0 system subscripts))
                                (add zero-point (vector-path axis-delta-list subscripts)))
                     result)))
    result))

(defmethod generate-connections ((system tonnetz) (zero-point point) axis-delta-list line-padding)
  (let ((result nil))
    (loop for connection in (connection-list system)
          do (let ((location-a (add zero-point
                                    (vector-path axis-delta-list
                                                 (get-offset-subscripts system
                                                                        (getf connection :a)))))
                   (location-b (add zero-point
                                    (vector-path axis-delta-list
                                                 (get-offset-subscripts system
                                                                        (getf connection :b))))))
               (push (make-line (move-point-towards location-a location-b line-padding)
                                (move-point-towards location-b location-a line-padding)
                                :style (getf connection :style))
                     result)))
    result))

(defmethod render ((system tonnetz) (origin point) axis-delta-list line-padding)
  (let ((zero-point (vector-path (mapcar #'invert-point axis-delta-list) (offsets system))))
    (make-group (append (generate-text-labels system zero-point axis-delta-list)
                        (generate-connections system zero-point axis-delta-list line-padding)))))





(defmethod cof ((center point) radius start-angle end-angle tick-object lbl-list lbl-offset id-offset)
  "`tick-object' needs to be relative to (0,0)."
  (make-circle-of-fifths (arc center radius start-angle end-angle)
                         (generate-positions center radius (length lbl-list) start-angle end-angle)
                         (generate-ticks center radius tick-object (length lbl-list) start-angle end-angle)
                         (generate-labels center radius lbl-list start-angle end-angle lbl-offset)
                         id-offset))





;; (defun list->array (lst)
;;   (make-array (length lst) :initial-contents lst))

;; (defun array->list (arr)
;;   (loop for e across arr collect e))

(defun simplify-ratio-list (ratio-list)
  (sort (remove-duplicates (mapcar (lambda (int) (/ int (apply #'gcd ratio-list))) ratio-list))
        #'>))

(defclass monochord (tonsystem)
  ((ratio-list :initarg :ratio-list :accessor ratio-list)))

(defun make-monochord (ratio-list)
  (make-instance 'monochord :ratio-list (simplify-ratio-list ratio-list)))


(defparameter *testmon* (make-monochord '(2 1)))

(defmethod add-ratio ((mon monochord) ratio &optional index)
  (setf (ratio-list mon)
        (simplify-ratio-list (cons (* (numerator ratio) (if index
                                                            (nth index (ratio-list mon))
                                                            (car (last (ratio-list mon)))))
                                   (mapcar (lambda (int)
                                             (* int (denominator ratio)))
                                           (ratio-list mon))))))

(defun proportion-string (ratio-list)
  (format nil "~{~a~^:~}" (simplify-ratio-list ratio-list)))

(defmethod render-monochord ((mon monochord) string-length tick-length)
  (with-accessors ((ratios ratio-list))
      mon

    (let* ((open-string (ln (pt 0 0) (pt string-length 0)))
           (locations (loop for int in ratios
                            collect (pt (- string-length
                                           (* (/ string-length (first ratios)) int))
                                        0)))
           (ticks (loop for location in (cons (pt string-length 0) locations)
                        collect (ln (below location tick-length) location)))
           (arcs (do ((remainder locations (rest remainder))
                      (num (ratio-list mon) (rest num))
                      (result nil))
                     ((null remainder) result)
                   (loop for i from 1 to (1- (length remainder))
                         do (push (make-arc-label (first remainder) (nth i remainder)
                                                  (proportion-string (list (first num)
                                                                           (nth i num)))
                                                  :height 0.7)
                                  result))))
           (title (make-text (proportion-string (ratio-list mon))
                             (add (first locations) (pt (* 3/4 string-length) 2))))
           )
      (gr (append (list open-string) ticks arcs (list title)
                  )))))
