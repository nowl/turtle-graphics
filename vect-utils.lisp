(in-package :turtle-graphics)

;;(declaim (optimize (safety 0) (speed 3)))

(defstruct vect
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (z 0.0 :type single-float))

(defstruct line
  slope offset)

(defclass color ()
  ((red :initarg :red :accessor red)
   (green :initarg :green :accessor green)
   (blue :initarg :blue :accessor blue)
   (min-color :initarg :min-color :accessor min-color)
   (max-color :initarg :max-color :accessor max-color)))

(defun dot (p1 p2)
  (+ (* (vect-x p1) (vect-x p2))
     (* (vect-y p1) (vect-y p2))
     (* (vect-z p1) (vect-z p2))))

(defun sub-vect (p1 p2)
  (make-vect :x (- (vect-x p1) (vect-x p2))
	     :y (- (vect-y p1) (vect-y p2))
	     :z (- (vect-z p1) (vect-z p2))))

(defun add-vect (p1 p2)
  (make-vect :x (+ (vect-x p1) (vect-x p2))
	     :y (+ (vect-y p1) (vect-y p2))
	     :z (+ (vect-z p1) (vect-z p2))))

(defun add-color (p1 p2)
  (make-instance 'color 
		 :red (+ (red p1) (red p2))
		 :green (+ (green p1) (green p2))
		 :blue (+ (blue p1) (blue p2))
		 :min-color (min (min-color p1) (min-color p2))
		 :max-color (max (max-color p1) (max-color p2))))

(defgeneric add (o1 o2 &rest others))
(defmethod add (o1 o2 &rest others)
  (apply #'common-lisp:+ o1 o2 others))
(defmethod add ((o1 vect) (o2 vect) &rest others)
  (declare (ignore others))
  (add-vect o1 o2))
(defmethod add ((o1 color) (o2 color) &rest others)
  (declare (ignore others))
  (add-color o1 o2))

(defgeneric subtract (o1 o2 &rest others))
(defmethod subtract (o1 o2 &rest others)
  (apply #'common-lisp:- o1 o2 others))
(defmethod subtract ((o1 vect) (o2 vect) &rest others)
  (declare (ignore others))
  (sub-vect o1 o2))

(defun cross (a b)
  (make-vect :x (- (* (vect-y a) (vect-z b))
		   (* (vect-z a) (vect-y b)))
	     :y (- (* (vect-z a) (vect-x b))
		   (* (vect-x a) (vect-z b)))
	     :z (- (* (vect-x a) (vect-y b))
		   (* (vect-y a) (vect-x b)))))


(defgeneric mult-by-scalar (obj scalar))
(defmethod mult-by-scalar ((obj vect) scalar)
  (make-vect :x (* (vect-x obj) scalar)
	     :y (* (vect-y obj) scalar)
	     :z (* (vect-z obj) scalar)))
(defmethod mult-by-scalar ((obj color) scalar)
  (make-instance 'color
		 :red (* (red obj) scalar)
		 :green (* (green obj) scalar)
		 :blue (* (blue obj) scalar)
		 :min-color (min-color obj)
		 :max-color (max-color obj)))

(defun sqr (num)
  (expt num 2))

(defun distance (p1 p2)
  (sqrt (+ (sqr (- (vect-x p1)
		   (vect-x p2)))
	   (sqr (- (vect-y p1)
		   (vect-y p2)))
	   (sqr (- (vect-z p1)
		   (vect-z p2))))))

(defun norm (p)
  (sqrt (+ (sqr (vect-x p))
           (sqr (vect-y p))
           (sqr (vect-z p)))))

(defun norm-vect (p)
  (let ((n (norm p)))
    (declare (single-float n))
    (make-vect :x (/ (vect-x p) n)
	       :y (/ (vect-y p) n)
	       :z (/ (vect-z p) n))))

(defun make-line-from-vects (p1 p2)
  (make-line :slope (norm-vect (sub-vect p2 p1))
             :offset p1))
(defun get-vect-at-time (line time)
  (+ (mult-by-scalar (line-slope line) time)
     (line-offset line)))

(defun vect-rotate (v rotate-vect angle &key (angle-units :radians))
  "This function will rotate the vector v around rotate-vect by angle
using quaternian rotations. v is the vector to rotate, rotate-vect is
the vector we are rotating around, angle is the rotation angle,
angle-units can be either :radians or :degrees specifiying the type of
angle"
  (let* ((n-rotate-vect (norm-vect rotate-vect))
	 (n-angle (ecase angle-units
		    (:radians angle)
		    (:degrees (* pi (/ angle 180)))))
	 (q-scalar (coerce (cos (/ n-angle 2)) 'single-float))
	 (sin-value (coerce (sin (/ n-angle 2)) 'single-float))
	 (q-vect (mult-by-scalar n-rotate-vect sin-value)))
    (let ((t1 (cross v q-vect))
	  (t2 (cross q-vect v))
	  (t4 (mult-by-scalar q-vect (dot q-vect v)))
	  (t6 (mult-by-scalar v (coerce (sqr q-scalar) 'single-float))))
      (let ((t3 (cross t2 q-vect))
	    (t5 (mult-by-scalar t2 q-scalar))
	    (t7 (mult-by-scalar t1 q-scalar)))
	(let ( ;;(res-scalar (dot t2 q-vect))
	      (res-vect (add (subtract (subtract (add t6 t5) t7) t3) t4)))
	  res-vect)))))

(defmacro make-generic-vector-rotation (rotation-vector)
  ``(vect-rotate ,vector ,',rotation-vector ,angle ,@args))

(defmacro rotate-vect-about-x (vector angle &rest args)
  (make-generic-vector-rotation (make-vect :x 1.0 :y 0.0 :z 0.0)))
(defmacro rotate-vect-about-y (vector angle &rest args)
  (make-generic-vector-rotation (make-vect :x 0.0 :y 1.0 :z 0.0)))
(defmacro rotate-vect-about-z (vector angle &rest args)
  (make-generic-vector-rotation (make-vect :x 0.0 :y 0.0 :z 1.0)))
