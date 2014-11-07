;;;; spacetime

;;;; I've previously modeled spacetime as a push-down stack of
;;;; "herenow" states. That's seemed clever at first. I like the idea
;;;; of the accumulator and the general idea of spacetime as emergent
;;;; property. However, here I'd like to explore some basic physics to
;;;; understand it better and seek out another clever idea about
;;;; spacetime.


;;; Relativity, special

(deftype speed ()
  `(rational 0 1))

(deftype range ()
  `(rational 0))

(defconstant +c+ 1/1
  "The speed of light")

(define-condition superluminal (error)
  ((object :reader object :initarg object))
  (:report (lambda (condition stream)
	     (format stream "Object ~A trying to break speed limit"
		     (object condition)))))



;;; 

;;; basis (vector)
;;; point
;;; event

;;; affine
;;; origin
;;; source
;;; vertex
;;; matrix

;;; bearing
;;; heading

;;; displace
;;; distance

;;; position
;;; velocity
;;; location
;;; interval

;;; direction
;;; magnitude
;;; reference
;;; spherical

;;; coordinate

;;; orientation
;;; rectilinear
;;; cylindrical

(defclass position ()
  (affine))

(defmethod distance ((p1 position) (p2 position))
  ())

(defclass velocity ()
  (bases :reader bases :initarg :bases
	 :documentation "basis vectors"))

(defclass rectilinear-v (velocity)
  ()
  (:documentation "Velocities which may be mapped over array subscripts.

This is a \"reverse Cartesian\" system (z, y, x) but should do what's
expected if your space is modeled in a Lisp array of the same number
of dimensions."))

(defmethod initialize-instance :before ((v rectilinear-v) :bases bases)
  (some (bases v)))

(defun make-velocity (&rest deltas)
  (make-instance 'velocity :deltas deltas))

(defun )



(defmethod delta-z ((v velocity))
  (first (deltas v)))

(defmethod delta-y ((v velocity))
  (second (deltas v)))

(defmethod delta-x ((v velocity))
  (third (deltas v)))




  
