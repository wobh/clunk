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


;;;; Support functions



(defun pushdiff (new-value place)
  "Push in new value if it's different from the current first value."
  (unless (eq new-value (first place))
    (push new-value place)))

(defun epushdiff (new-value place)
  "Push in new value if it's different from the current first value. Raise an error if not."
  (if (not (eq new-value (first place)))
      (push new-value place)
      (error "new-value ~S is the same as current ~S" new-value (first place))))

;;;; 

(defclass being ()
  ((being-type :reader being-type :initarg :type)
   (states :accessor states)))

;;;; Worlds as classes

(defclass world ()
  ((things :accessor things :initform (make-hash-table))
   (places :accessor places :initform ())))

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

(defun make-thing (name place world)
  ())

(defparameter *world* (make-instance 'world))

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

(defmethod walk ((agent agent) direction)
  (use agent (legs agent)))

(defmethod use ((agent agent) (legs legs) (walk end) direction &rest)
  ())


(defclass limb (means) ())

(defclass arm (limb)
  ((hand :accessor hand)))

(defclass hand ())
