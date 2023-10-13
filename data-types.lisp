(in-package :drawer)


(defclass drawer-object ()
  ())


(defclass scalar (drawer-object)
  ((value :initarg :value :accessor value)))

(defmethod print-object ((object scalar) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "value: ~a" (value object))))

(defmethod make-scalar (value)
  (make-instance 'scalar :value value))


(defclass point (drawer-object)
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)))

(defmethod print-object ((object point) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "x: ~a, y: ~a" (x object) (y object))))

(defmethod make-point (x y)
  (make-instance 'point :x x :y y))


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

(defun get-type-of-elements (element-list)
  (remove-duplicates (loop for element in element-list collect (type-of element))))

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



(defclass structuring-object (visible-object)
  ())

(defclass group (structuring-object)
  ((content :initarg :content :accessor content)))

(defmethod print-object ((object group) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "content: ~a" (content object))))

(defmethod make-group (content)
  (make-instance 'group :content content))




(defgeneric copy-move (drawer-object line))

(defmethod copy-move ((obj point) (delta point))
  (make-point (make-scalar (+ (value (x obj)) (value (x delta))))
      (make-scalar (+ (value (y obj)) (value (y delta))))))

(defmethod copy-move ((obj line) (delta point))
  (make-line (copy-move (origin obj) delta)
      (copy-move (destination obj) delta)))

(defmethod copy-move ((obj group) (delta point))
  (make-group (loop for element in (content obj)
                    collect (copy-move element delta))))

(defmethod copy-move ((obj line-strip) (delta point))
  (make-line-strip (loop for point in (point-list obj)
                         collect (copy-move point delta))))


(defmethod above ((origin point) (distance scalar))
  (make-point (x origin) (make-scalar (+ (value (y origin)) (value distance)))))

(defmethod below ((origin point) (distance scalar))
  (make-point (x origin) (make-scalar (- (value (y origin)) (value distance)))))

(defmethod right-of ((origin point) (distance scalar))
  (make-point (make-scalar (+ (value (x origin)) (value distance))) (y origin)))

(defmethod left-of ((origin point) (distance scalar))
  (make-point (make-scalar (- (value (x origin)) (value distance))) (y origin)))







(defclass drawer-backend ()
  ())


(defclass backend-text (drawer-backend)
  ((filename :initarg :filename :accessor filename)))


(defgeneric draw (visible-object backend))



(defmethod draw ((obj line) (backend backend-text))
  (format t "~&Drawing a line from (~a,~a) to (~a,~a)"
          (value (x (origin obj)))
          (value (y (origin obj)))
          (value (x (destination obj)))
          (value (y (destination obj)))))

(defmethod draw ((obj line-strip) (backend backend-text))
  )


(defmethod draw ((obj group) (backend backend-text))
  (format t "~&Drawing a group of ~a objects:"
          (length (content obj)))
  (dolist (element (content obj))
    (draw element backend)))




(let* ((a (make-scalar 15))
       (b (make-scalar 17))
       (p (make-point a b))
       (c (make-scalar -5))
       (d (make-scalar -7))
       (pp (make-point c d))
       (l (make-line p pp))
       (dd (make-scalar 300))
       (ll (make-line (above p dd) (above pp dd)))
       (g (make-group (list l ll)))
       (x (make-scalar 1000))
       (delta (make-point x x))
       (cg (copy-move g delta))
       (ls (make-line-strip (list p pp (above p dd) (below pp dd))))
       (cls (copy-move ls delta))
       (tb (make-instance 'backend-text)))
  (format t "~&~a" ls)
  (format t "~&~a" cls)
  (draw ll tb)
  (draw g tb)
  (draw cg tb))
