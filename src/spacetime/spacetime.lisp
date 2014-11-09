;;;; spacetime


;;; Relativity, special

(defconstant +c+ 1/1
  "The speed of light")

(define-condition superluminal (error)
  ((object :reader object :initarg :object))
  (:report (lambda (condition stream)
	     (format stream "Object ~A is trying to break the speed limit."
		     (object condition)))))

(deftype speed ()
  "A valid speed is a Rational number between 0 and 1."
  `(rational 0 +c+))



(defun anglep (n)
  "Test if n is a valid angle."
  (typep n '(rational 0 1)))

(deftype angle ()
  "A valid angle is a Rational number between 0 and 1."
  `(satisfies anglep))

(defun angle= (a1 a2)
  "Compare two angles."
  (check-type a1 'angle)
  (check-type a2 'angle)
  (or (= a1 a2)
      (and (= a1 0) (= a2 1))
      (and (= a1 1) (= a2 0))))

(defun coords-polar-p (coords)
  (and
   (integerp (elt chords 0))
   (every #'anglep (subseq seq 1))))

(defun coords-point-p (coords)
  (every #'integerp coords))

(defun coords-range-p (coords)
  (every #'rationalp coords))

;;; Have to implement two basic concepts: position and momentum.

(defclass position ()
  ((coordinates :reader coords :initarg :coords))
  (:documentation "A sequence of one or more numbers, called coordinates
The coordinates in the sequence may be of types:

- either integers or ratios (not both mixed)
  + representing coordinates/points in a rectilinear volume.
  + each number is a distance from origin point (0) along orthagonal axes.
  + number of points indicates dimensions of volume (usually 1 <= d <= 3).

- one integer followed by one or more ratios between 0 and 1
  + the integer represents a distance from an origin point.
  + the ratios represent angular coordinates for orthagonal axis planes.
  + the number of values represents the number of dimensions in the volume.

In either case the order in which coordinates map to axes is up to the user."))

(defun make-position (&rest coords)
  (make-instance 'position :coords coords))

(defmethod distance ((p1 position) (p2 position))
  "The interval of positions"
  (cond ((every coords-range-p (concatenate 'list (coords p1) (coords p2)))
	 (let (interval variance)
	   (if (< (length p1) (length p2))
	       (setf interval (copy-seq (coordinates p2))
		     variance p1)
	       (setf interval (copy-seq (coordinates p1))
		     variance p2))
	   ((map-into interval #'- variance))))))





;;; momentum

(defclass momentum ()
  ((mass)))
    
;;; velocity

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


;;; Topics
;;; * Momentum
;;;   - http://en.wikipedia.org/wiki/Momentum
;;; * Special Relativity
;;;   - http://en.wikipedia.org/wiki/Special_relativity
;;; * Basis
;;;   - http://en.wikipedia.org/wiki/Basis_(linear_algebra)
;;; * Rapidity
;;;   - http://en.wikipedia.org/wiki/Rapidity
;;;   - http://en.wikipedia.org/wiki/Velocity-addition_formula
;;; * Coordinate systems
;;;   - http://en.wikipedia.org/wiki/Coordinate_system
;;;   - http://en.wikipedia.org/wiki/Cartesian_coordinate_system
;;;   - http://en.wikipedia.org/wiki/Polar_coordinate_system
;;;   - http://en.wikipedia.org/wiki/Spherical_coordinate_system
;;;   - http://en.wikipedia.org/wiki/Curvilinear_coordinates
;;;   - http://en.wikipedia.org/wiki/Homogeneous_coordinates
;;; * Higher dimensions
;;;   - http://en.wikipedia.org/wiki/N-sphere
;;;   - http://en.wikipedia.org/wiki/Hypercube
;;; * Space
;;;   - http://en.wikipedia.org/wiki/Affine_space
;;;   - http://en.wikipedia.org/wiki/Position_(vector)
;;;   - http://en.wikipedia.org/wiki/Hyperbolic_space
;;;   - http://en.wikipedia.org/wiki/Minkowski_space
;;;   - http://en.wikipedia.org/wiki/Distance
;;;   - http://en.wikipedia.org/wiki/Norm_(mathematics)
;;; * Constants
;;;   - http://en.wikipedia.org/wiki/Physical_constant
;;;   - http://en.wikipedia.org/wiki/Natural_units
;;;   - http://en.wikipedia.org/wiki/Planck_units
;;;   - http://www.tauday.com/tau-manifesto
;;; * Complements
;;;   - http://en.wikipedia.org/wiki/Complementarity_(physics)
;;;   - 

;;; Naming things is hard
;;; TODOThis could be adapted for Lexicode

;;; time
;;; mass
;;; axis/es
;;; span

;;; basis/es (vector)
;;; point
;;; event
;;; speed
;;; range
;;; frame
;;; space
;;; field

;;; length
;;; volume
;;; affine
;;; origin
;;; source
;;; vertex
;;; matrix
;;; vector
;;; scalar
;;; matter
;;; extent/d

;;; bearing
;;; heading

;;; duration
;;; displace
;;; distance
;;; position
;;; rapidity
;;; velocity
;;; location
;;; interval

;;; direction
;;; dimension
;;; magnitude
;;; reference
;;; spherical

;;; coordinate

;;; orientation
;;; rectilinear
;;; cylindrical

