(in-package :turtle-graphics)

(defstruct turtle
  pos
  direction
  pen)

(defun reset-turtle (turtle)
  (setf (turtle-pos turtle) '(0 0 0)
        (turtle-direction turtle) '(1.0 0.0 0.0)
        (turtle-pen turtle) '(1.0 1.0 1.0)))       

#|
(defparameter *test-control*
  '(:string "faf+f-f"
    :angle 35))
|#

(defun turtle-single-control (turtle char angle)
  (case char
    (#\f (let ((start (turtle-pos turtle))
               (end (mapcar #'+
                            (turtle-pos turtle)
                            (turtle-direction turtle))))
           (setf (turtle-pos turtle) end)
           (list (turtle-pen turtle) start end)))
    (#\+ (setf (turtle-direction turtle)
               (destructuring-bind (x y z) (turtle-direction turtle)
                 (let ((rot-vect (rotate-vect-about-z (make-vect :x x :y y :z z) angle)))
                   (list (vect-x rot-vect) (vect-y rot-vect) (vect-z rot-vect)))))
                 
         nil)
    (#\- (setf (turtle-direction turtle)
               (destructuring-bind (x y z) (turtle-direction turtle)
                 (let ((rot-vect (rotate-vect-about-z (make-vect :x x :y y :z z) (- angle))))
                   (list (vect-x rot-vect) (vect-y rot-vect) (vect-z rot-vect)))))
         nil)
    (t nil)))

(defun turtle-control (turtle control)
  (let ((string (cadr (member :string control)))
        (angle (cadr (member :angle control)))
        (color (cadr (member :color control))))
    (setf (turtle-pen turtle) color)
    (delete nil
            (loop for char across string collect
                 (turtle-single-control turtle char angle)))))
