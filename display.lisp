(in-package :turtle-graphics)

(defparameter *vectors*
  (loop for i below 100 collect
       (list (list (random 1.0) (random 1.0) (random 1.0))
             (list (random 10.0) (random 10.0) (random 10.0))
             (list (random 10.0) (random 10.0) (random 10.0)))))

(defstruct camera-state
  x y z look-x look-y look-z
  (alpha-angle 0)
  (theta-angle 0))

(defparameter *mouse-rotation-sensitivity* 500)
(defparameter *mouse-movement-sensitivity* 100)

(defparameter *camera*
  (make-camera-state :x 0.0 :y 0.0 :z 5.0 
                     :look-x 0.0 :look-y 0.0 :look-z 0.0))

;; This function will change x, y, and z based on alpha-angle and
;; theta-angle
(defun set-camera-location-from-angles (camera)
  (let ((radius (sqrt (+ (expt (- (camera-state-x camera) (camera-state-look-x camera)) 2)
                         (expt (- (camera-state-y camera) (camera-state-look-y camera)) 2)
                         (expt (- (camera-state-z camera) (camera-state-look-z camera)) 2)))))
    (setf (camera-state-y camera) (+ (camera-state-look-y camera)
                                     (* radius (sin (camera-state-theta-angle camera))))
          (camera-state-x camera) (+ (camera-state-look-x camera)
                                     (* radius 
                                        (sin (camera-state-alpha-angle camera))
                                        (cos (camera-state-theta-angle camera))))
          (camera-state-z camera) (+ (camera-state-look-z camera)
                                     (* radius
                                        (cos (camera-state-alpha-angle camera))
                                        (cos (camera-state-theta-angle camera)))))))

(defun set-camera-location-with-radius-change (camera radius-change)
  (let ((radius (* (sqrt (+ (expt (- (camera-state-x camera) (camera-state-look-x camera)) 2)
                            (expt (- (camera-state-y camera) (camera-state-look-y camera)) 2)
                            (expt (- (camera-state-z camera) (camera-state-look-z camera)) 2)))
                   radius-change)))
    (setf (camera-state-y camera) (+ (camera-state-look-y camera)
                                     (* radius (sin (camera-state-theta-angle camera))))
          (camera-state-x camera) (+ (camera-state-look-x camera)
                                     (* radius 
                                        (sin (camera-state-alpha-angle camera))
                                        (cos (camera-state-theta-angle camera))))
          (camera-state-z camera) (+ (camera-state-look-z camera)
                                     (* radius
                                        (cos (camera-state-alpha-angle camera))
                                        (cos (camera-state-theta-angle camera)))))))


(defclass glut-turtle-graphics-window (glut:window)
  ()
  (:default-initargs :width 1024 :height 1024 :title "Turtle Graphics Sim"
                     :mode '(:single :rgb :depth)))

(defmethod glut:display-window :before ((w glut-turtle-graphics-window))
  (gl:clear-color 0 0 0 0)
  (gl:cull-face :back)
  (gl:depth-func :less)
  (gl:disable :dither)
  (gl:shade-model :smooth)
  (gl:light-model :light-model-local-viewer 1)
  (gl:color-material :front :ambient-and-diffuse)
  (gl:enable :light0 :lighting :cull-face :depth-test))

(defmethod glut:display ((window glut-turtle-graphics-window))  
  (gl:load-identity)
  (glu:look-at (camera-state-x *camera*)
               (camera-state-y *camera*)
               (camera-state-z *camera*)
               (camera-state-look-x *camera*)
               (camera-state-look-y *camera*)
               (camera-state-look-z *camera*)
               0 1 0)
  (gl:light :light0 :position '(0 1 1 0))
  ;(gl:light :light0 :diffuse '(0.2 0.4 0.6 0))
  (gl:clear :color-buffer :depth-buffer)
  ;;(gl:front-face :cw)
  (loop for vector in *vectors* do
       (let ((color (first vector))
             (p1 (second vector))
             (p2 (third vector)))
         ;(apply #'gl:color color)
         (gl:material :front :ambient-and-diffuse `#(,(first color)
                                                     ,(second color) 
                                                     ,(third color)
                                                     1.0))
         (gl:with-primitives :lines
           (apply #'gl:vertex p1)
           (apply #'gl:vertex p2))))
  ;;(gl:front-face :ccw)
  (gl:flush))

(defmethod glut:reshape ((window glut-turtle-graphics-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 60 (/ width height) 0.1 1000)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defmethod glut:keyboard ((window glut-turtle-graphics-window) key x y)
  (declare (ignore x y))
  (when (eql key #\Esc)
    (glut:destroy-current-window)))

(defparameter *mouse-down* nil)

(defmethod glut:mouse ((window glut-turtle-graphics-window) button state x y)
  (cond
    ((and (eql button :wheel-up) (eql state :down))
     (set-camera-location-with-radius-change *camera* .9)
     (glut:post-redisplay))
    ((and (eql button :wheel-down) (eql state :down))
     (set-camera-location-with-radius-change *camera* 1.1)
     (glut:post-redisplay))
    ((and (eql button :left-button) (eql state :down)) (setf *mouse-down* (list :left-button x y)))
    ((and (eql button :left-button) (eql state :up)) (setf *mouse-down* nil))
    ((and (eql button :right-button) (eql state :down)) (setf *mouse-down* (list :right-button x y)))
    ((and (eql button :right-button) (eql state :up)) (setf *mouse-down* nil))))

(defmethod glut:motion ((window glut-turtle-graphics-window) x y)
  (when (not (null *mouse-down*))
    (destructuring-bind (button start-x start-y) *mouse-down*
      (cond ((eql :left-button button)
             (incf (camera-state-theta-angle *camera*)
                   (/ (- y start-y) *mouse-rotation-sensitivity*))
             (incf (camera-state-alpha-angle *camera*)
                   (/ (- x start-x) *mouse-rotation-sensitivity*))
             (set-camera-location-from-angles *camera*)
             (setf *mouse-down* (list button x y))
             (glut:post-redisplay))
            ((eql :right-button button) 
             (let ((dx (/ (- x start-x) *mouse-movement-sensitivity*))
                   (dy (/ (- y start-y) *mouse-movement-sensitivity*)))
               (let ((incy (* dy (cos (camera-state-theta-angle *camera*))))
                     (incx  (* -1 dx (cos (camera-state-alpha-angle *camera*))))
                     (incz  (* dx (sin (camera-state-alpha-angle *camera*)))))
                 (incf (camera-state-y *camera*) incy)
                 (incf (camera-state-look-y *camera*) incy)
                 (incf (camera-state-x *camera*) incx)
                 (incf (camera-state-look-x *camera*) incx)
                 (incf (camera-state-z *camera*) incz)
                 (incf (camera-state-look-z *camera*) incz)
                 (setf *mouse-down* (list button x y))
                 (glut:post-redisplay))))))))

(defun turtle-graphics-run ()
  (glut:display-window (make-instance 'glut-turtle-graphics-window)))