;; -*- mode: lisp

;;;; FILE: scratch.lisp

;;;; AUTHORS:

;;;; William Clifford <wobh@yahoo.com>

;;;; DESCRIPTION:

;;;; Lower-Earth scratch file

;;;; NOTES

;;;; I like vertical alignment so I tend to favor names and keywords
;;;; that all have the same length.

;;;; Some of the vocabulary I'm trying to work with



(defpackage "ORG.WOBH.LOWER-EARTH.SCRATCH"
  (:nicknames "SCRATCH")
  (:use "COMMON-LISP")
  (:export "TEST")
  (:documentation
   "A scratch package for lower earth experiments."))

(in-package "ORG.WOBH.LOWER-EARTH.SCRATCH")


;;; Events

(defclass event ()
  ((state :reader what  :initarg :what)
   (locus :reader where :initarg :where)))

(defun make-event (state locus)
  (make-instance 'event :what state :where locus))

(defmethod event= ((e1 event) (e2 event))
  "Compare two events"
  (and (equalp (what e1) (what e2))
       (= (where e1) (where e2))))

;; NOTE: Where's when? When is the position in History. See below.


;;; History

;; History as pushdown stack, where adjacent event values must be
;; different in some sense.


;; support functions

(defun pushdiff (new-value place &optional (test 'eq) (accessor 'first))
  "Push in new value if it's different from the current first value."
  (unless (funcall test new-value (funcall accessor place))
    (push new-value place)))

(defun epushdiff (new-value place &optional (test 'eq) (accessor 'first))
  "Push in new value if it's different from the current first value. Raise an error if not."
  (unless (pushdiff new-value place test accessor)
    (error "new-value ~S is the same as current ~S" new-value (first place))))

;;; History

(defclass history ()
  ((events :reader events)))

(defmethod now ((history history))
  (state (first (events history))))

(defmethod here ((history history))
  (locus (first (events history))))

(defmethod record-event ((history history) event)
  (check-type (state event) (state (now history)))
  (epushdiff event (events history)))

(defmethod events-since ((history history) start)
  (nreverse
   (loop
      for event in (events history)
      collect event
      until (equal event start))))


;; NOTES: I've modeled time as a push-down stack of accumulating
;; events with "now" being the latest event on the stack. Each "being"
;; is a history of these events. But in what way to the histories
;; interact?

;; This model doesn't accomodate space except in the vague sense of a
;; "locus" which I first thought as a generic term for accumulating
;; positions. I'm unsatisfied with this, in part because, to the best
;; of my limited understanding of their ideas, both Einstein and
;; Heisenberg's Big idea's suggest that thinking "positionally" is an
;; illusion.

;; I perceive what the world is to me, but I'm not independant of the
;; world in any way, so what am I to the world?



;;; Beings

;; A being is a type of event. We should be able to add slots to it.

(defmacro defbeing (name &optional superclasses slots initargs documentation)
  ;; FIXME
  (let ((maker (make-symbol (format nil "make-~a" name))))
    `(prog1
	 (defclass ,name (events ,@superclasses)
	   ,slots ,@initargs ,@documentation)
       (defun ,maker ()
	 (make-instance (quote ,name))))))



;;; Creatures

;; A being may be the state of an actor or agent of some kind. It can
;; think on it's current state and act. Not sure how the macro above
;; should act this shall be implemented directly.

(defclass agency (event)
  ((health :reader health :initarg :health)))

(defun make-agency (state locus health)
  (make-instance 'creature :what state :where locus :health health))

(defclass creature (history)
  ())

(defmethod wound-creature ((creature creature) (place place))
  (let (impending (make-agency place (health now creature)))
    (decf (health impending))
    (record-event creature impending)))

(defun creature-alive-p ((creature creature))
  (< 0 (health (now creature))))







;;; Worlds

(defclass world ()
  ((things :accessor things :initform (make-hash-table))
   (places :accessor places :initform ()))
  (:documentation "A world has places and things"))

(defmethod make ((world world) name &optional value)
  (with-accessors ((things things)) world
    (setf (gethash name things)
          (when value
            (list value)))))

(defmethod show ((world world) name)
  (with-accessors ((things things)) world
    (first (gethash name things))))

(defmethod edit ((world world) name value)
  (with-accessors ((things things)) world
    (pushdiff value (gethash name things))))

(defmethod kill ((world world) name)
  (with-accessors ((things things)) world
    (remhash name things)))

(defparameter *world* (make-instance 'world))



;;; Things



;;; meh

(defun setup-adv ()
  (make *world* adv-str)
  (make *world* adv-dex)
  (make *world* adv-int))



;;;; asdf

(defmacro defhistory-slot (slot-def)
  (destructuring-bind (slot-name &rest slot-body) slot-def
    `(quote
      (,slot-name :accessor ,slot-name
                  :initarg ,(intern (string-upcase slot-name) 'keyword)
                  ,@slot-body))))

(defmacro defhistory (name superclasses slot-defs
                      &optional &rest options)
  `(prog1
       (defclass ,name ,superclasses
         ,(loop
             for slot-def in slot-defs
             append (defhistory-slot slot-def))
         ,@options)
     (defmethod )))

(defclass history ()
  ((events :accessor events :initarg :events :initform ())))

(defmethod (setf events) ((history history) event)
  (with-slots (events) history
    (push event events)))

(defun test (scratchf &rest args &key &allow-other-keys)
  (funcall scratchf args))

(defclass attr ()
  ((attr-values :accessor attr)))

(defclass place ()
  ((beings :accessor beings :initarg :beings))
  (:default-initargs :beings ()))

(defun make-place (&optional &rest beings)
  (make-instance 'place :beings beings))

(defmethod add-being ((b being) (p place))
  (with-accessors ((bs beings)) p
    (pushnew b bs)))

(defmethod del-being ((b being) (p place))
  (with-accessors ((bs beings)) p
    (delete b bs)))



(defclass level ()
  ((places :accessor places :initarg :places)))

(defun make-level (y-size x-size)
  (let ((places (make-array (y-size x-size))))
    (loop
       for i below (array-total-size places)
       do (setf (row-major-aref places i) (make-place)))
    (make-instance 'level :places places)))

(defmethod beings-at ((lvl level) y-coord x-coord)
  (beings (aref (places lvl) y-coord x-coord)))

(defmethod move-being ((lvl level) (b being) y-coord x-coord)
  (add-being (beings-at lvl y-coord x-coord))
  (del-being (beings-at)))


(defmethod show-level ((lvl level) stream)
  (loop
     for y below (first (array-dimensions lvl))
     do
       (loop
          for x below (second (array-dimensions lvl))
          do
            (let ((beings (beings-at lvl y x)))
              (write-icon (find-iconable beings))))))


(defclass event ()
  ((name :reader name-of :initarg :name)
   (info :type list :initarg :info)))

;;; agent has various means which have various uses
;;;

(defclass agent () ())
(defclass means () ())
(defclass end   () ())

(defgeneric use (agent means end &rest args)
  :documentation "An AGENT USEs MEANS to try END.")

(defmethod use :around ((agent agent) (means means) (end end) &rest args)
  (unless (and (available-p (means agent))
               (appropriate-p (means agent) end))
    (call-next-method)))

(defmethod walk ((agent agent) direction speed)
  (use agent (legs agent) move direction speed))

(defmethod use ((agent agent) (legs legs) (walk end) direction &rest)
  ())


(defclass limb (means) ())

(defclass arm (limb)
  ((hand :accessor hand)))

(defclass hand ())
