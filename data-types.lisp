(in-package :drawer)


(defclass drawer-object ()
  ())


(defparameter *global-scale-factor* 1)

(defclass scalar (drawer-object)
  ((value :initarg :value :accessor get-value)))

(defmethod print-object ((object scalar) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "value: ~a" (value object))))

(defmethod make-scalar (value)
  (make-instance 'scalar :value value))

(defmethod value ((sclr scalar))
  (with-accessors ((val get-value))
      sclr
    (* *global-scale-factor* val)))

(defmethod add ((a scalar) (b scalar))
  (make-scalar (+ (value a) (value b))))

(defmethod subtract ((a scalar) (b scalar))
  (make-scalar (- (value b) (value a))))

(defmethod scale ((s scalar) factor)
  (make-scalar (* factor (value s))))



(defparameter *global-point-transformer* (lambda (x) x))

(defclass point (drawer-object)
  ((x :initarg :x :accessor get-x)
   (y :initarg :y :accessor get-y)))

(defmethod print-object ((object point) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "x: ~a, y: ~a" (x object) (y object))))

(defmethod make-point (x y)
  (make-instance 'point :x x :y y))

(defmethod x ((p point))
  (get-x (funcall *global-point-transformer* p)))

(defmethod y ((p point))
  (get-y (funcall *global-point-transformer* p)))

(defmethod add ((a point) (b point))
  (make-point (add (x a) (x b))
      (add (y a) (y b))))

(defmethod subtract ((a point) (b point))
  (make-point (subtract (x b) (x a))
      (subtract (y b) (y a))))

(defmethod scale ((p point) factor)
  (make-point (scale (x p) factor)
      (scale (y p) factor)))



(defclass visible-object (drawer-object)
  ())


(defclass line (visible-object)
  ((origin :initarg :origin :accessor origin)
   (destination :initarg :destination :accessor destination)))

(defmethod print-object ((object line) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "origin: ~a, destination: ~a" (origin object) (destination object))))

(defmethod make-line (origin destination)
  (make-instance 'line :origin origin :destination destination))




(defun extract-value-list (point-list)
  (loop for point in point-list
        append (list (value (x point))
                     (value (y point)))))

(defclass line-strip (visible-object)
  ((point-list :initarg :point-list :accessor point-list)))

(defmethod print-object ((object line-strip) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "Line-strip: [~{(~a, ~a) ~}]"
            (extract-value-list (point-list object)))))

(defmethod make-line-strip (point-list)
  (format t "~&~a" (get-type-of-elements point-list))
  (if (equal (get-type-of-elements point-list) (list 'point))
      (make-instance 'line-strip :point-list point-list)
      (error "Point-list ~a does not consist of exclusively points." point-list)))




(defclass circle (visible-object)
  ((radius :initarg :radius :accessor radius)
   (center :initarg :center :accessor center)))

(defmethod print-object ((object circle) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "center: (~a,~a), radius: ~a"
            (value (x (center object)))
            (value (y (center object)))
            (value (radius object)))))

(defmethod make-circle ((center point) (radius scalar))
  (make-instance 'circle :center center :radius radius))




(defclass text (visible-object)
  ((text-string :initarg :text-string :accessor text-string)
   (anchor :initarg :anchor :accessor anchor)
   (horizontal-alignment :initarg :horizontal-alignment :accessor horizontal-alignment)
   (vertical-alignment :initarg :vertical-alignment :accessor vertical-alignment)))

(defmethod make-text (text-string (anchor point) &key (h-align :center) (v-align :center))
  (make-instance 'text :text-string text-string
                       :anchor anchor
                       :horizontal-alignment h-align
                       :vertical-alignment v-align))







(defclass structuring-object (visible-object)
  ())

(defclass group (structuring-object)
  ((content :initarg :content :accessor content)))

(defmethod print-object ((object group) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "content: ~a" (content object))))

(defmethod make-group (content)
  (make-instance 'group :content content))
