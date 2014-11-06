;;;; WARPS - Will's Awesome Role-Playing System

;;;; AUTHORS
;;;; William Clifford whc wobh@yahoo.com

;;;; DESCRIPTION

;;;; WARPS lisp definitions


(defclass modifier ()
  ((name
    :reader   name
    :initarg :name)
   (value
    :accessor value
    :initarg :value)
   (conditions
    :reader   conditions
    :initarg :conditions)))

;;; TODO: rewrite as proper decorator. Probably these will be :AROUND
;;; methods.

(defclass attribute ()
  ((name
    :reader   name
    :initarg :name)
   (value
    :accessor value
    :initarg :value))
  ((modifiers
    :accessor modifiers
    :initarg :modifiers)))

;;; TODO: rethink this: are attributes merely value cells?

(defclass adventurer ()
  ((names
    :accessor names
    :initarg :names)
   (attributes
    :accessor attributes
    :initarg :attributes)))

;;; TODO: rethink this: are adventurers composed of their attributes?
