(in-package :drawer)

(defun make-point (x-coord y-coord)
  (cons x-coord y-coord))

(defun get-x-coord (point)
  (car point))

(defun get-y-coord (point)
  (cdr point))


(defun make-line (point-a point-b)
  (cons point-a point-b))

(defun get-point-a (line)
  (car line))

(defun get-point-b (line)
  (cdr line))


(defun make-circle (point-center radius)
  (cons point-center radius))

(defun get-circle-center (circle)
  (car circle))

(defun get-circle-radius (circle)
  (cdr circle))


(defun make-text (text-string point-position)
  (cons text-string point-position))

(defun get-text-string (text)
  (car text))

(defun get-point-position (text)
  (cdr text))

(defun )
