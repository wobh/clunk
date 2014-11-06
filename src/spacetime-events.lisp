;;;; spacetime

;;;; I've previously modeled spacetime as a push-down stack of
;;;; "herenow" states. That's seemed clever at first. I like the idea
;;;; of the accumulator and the general idea of spacetime as emergent
;;;; property. However, here I'd like to explore some basic physics to
;;;; understand it better and seek out another clever idea about
;;;; spacetime.


;;; Relativity, special

(defconstant +c+ 1/1
  "The speed of light")

(defclass velocity ()
  ((deltas :reader deltas :initarg :deltas))
  (:documentation "Provides a slot `deltas' to hold a sequence of coordinate changes."))

(defun make-velocity (&rest deltas)
  (make-instance 'velocity :deltas deltas))

(defmethod delta-t ((v velocity))
  (first (deltas v)))

(defmethod delta-z ((v velocity))
  (second (deltas v)))

(defmethod delta-y ((v velocity))
  (third (deltas v)))

(defmethod delta-x ((v velocity))
  (fourth (deltas v)))

(defmethod delta-n ((v velocity))
  (rest ()))




  
