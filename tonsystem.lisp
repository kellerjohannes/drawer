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
