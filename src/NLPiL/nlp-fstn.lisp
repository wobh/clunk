;;;; Natural Language Processing in lisp

;;;; Finite State Transition Networks FSTN

(in-package "CL-USER")

(defpackage "FSTN"
  (:use "CL")
  (:documentation "Natural Language Processing in Lisp
Chapter 2: Traversing FSTN in Lisp"))

(in-package "FSTN")

(defun to-laugh (tape)
  "NLPL 2.3, page 3.7."
  (prog ((newtape tape))
     label-1
     (case (car newtape)
       (h (setq newtape (cdr newtape)) (go label-2))
       (otherwise (return Nil)))
     label-2
     (case (car newtape)
       (a (setq newtape (cdr newtape)) (go label-3))
       (otherwise (return Nil)))
     label-3
     (case (car newtape)
       (! (setq newtape (cdr newtape)) (go label-4))
       (h (setq newtape (cdr newtape)) (go label-2))
       (otherwise (return Nil)))
     label-4
     (if (null newtape)
	 (return T)
	 (return Nil))))

(defparameter *swahili-1*
  '((Initial (1))
    (Final (5))
    (From 1 to 2 by subj)
    (From 2 to 3 by tense)
    (From 3 to 4 by obj)
    (From 4 to 5 by stem)))



(defparameter *english-1*
  '((Initial (1))
    (Final (9))
    (From 1 to 3 by NP)
    (From 1 to 2 by DET)
    (From 2 to 3 by N)
    (From 3 to 4 by BV)
    (From 4 to 5 by ADV)
    (From 4 to 5 by |#|)
    (From 5 to 6 by DET)
    (From 5 to 7 by DET)
    (From 5 to 8 by |#|)
    (From 6 to 6 by MOD)
    (From 6 to 7 by ADJ)
    (From 7 to 9 by N)
    (From 8 to 8 by MOD)
    (From 8 to 9 by ADJ)
    (From 9 to 4 by CNJ)
    (From 9 to 1 by CNJ)))

(defparameter *eng-monosyl*
  '((Initial (1))
    (Final (3 4 5))
    (From 1 to 2 by C0)
    (From 2 to 3 by V)
    (From 3 to 4 by C8)
    (From 4 to 5 by s)
    (From 1 to 7 by C3)
    (From 7 to 3 by C2)
    (From 7 to 2 by w)
    (From 1 to 6 by C2)
    (From 6 to 2 by l)
    (From 6 to 5 by |#|)
    (From 1 to 5 by C1)
    (From 5 to 2 by r)
    (From 1 to 8 by s)
    (From 8 to 5 by C4)
    (From 8 to 2 by C5)
    (From 3 to 9 by l)
    (From 3 to 10 by s)
    (From 3 to 11 by C7)
    (From 9 to 4 by C6)
    (From 10 to 4 by C4)
    (From 11 to 4 by th)))

(defparameter *monos*
  '((V a ae ai au e ea ee ei eu i ia ie o oa oe oi oo ou ue ui)
    (C0 bc ch d f g h j k l m n p qu r s sh t th v w x)
    (C1 d sh th)
    (C2 bc f g k)
    (C3 d g h t th)
    (C4 c k p t)
    (C5 c k l m n p pl qu t w)
    (C6 b f m)
    (C7 d f l n x)
    (C8 b c ch ck d f g h k l m mp mph n ng p que r s sh th v w x y z)))

(defparameter *abbreviations*
  '((NP kim sandy lee)
    (DET a the her)
    (N consumer man woman)
    (BV is was)
    (CNJ and or)
    (ADJ happy stupid)
    (MOD very)
    (ADV often always sometimes)))

(defun initial-nodes (network)
  (nth 1 (assoc 'Initial network)))

(defun final-nodes (network)
  (nth 1 (assoc 'Final network)))

(defun transitions (network)
  (cddr network))

(defun trans-node (transition)
  (getf transition 'From))

(defun trans-newnode (transition)
  (getf transition 'to))

(defun trans-label (transition)
  (getf transition 'by))

(defun recognize-move (label tape)
  (if (equal label (car tape))
      (list (cdr tape))
      (if (member (car tape) (assoc label *abbreviations*))
	  (list (cdr tape))
	  (if (equal label '|#|)
	      (list tape)
	      '()))))

(defun recognize-next (node tape network)
  "Throws T or returns Nil."
  (if (and (null tape) (member node (final-nodes network)))
      (throw 'stop T) ; Success
      (dolist (transition (transitions network))
	;; Try each transition of the network
	(if (equal node (trans-node transition)) ; If it starts at the right node
	    (dolist (newtape (recognize-move (trans-label transition) tape))
	      ;; Try each possible new value of tape
	      (recognize-next (trans-newnode transition) newtape network))))))

(defun recognize (network tape)
  "Returns T if successfully recognizes tape -- Nil otherwise."
  (catch
      'stop
    (dolist (initial-node (initial-nodes network))
      (recognize-next initial-node tape network))
    Nil))

(defun generate-move (label tape)
  (if (equal label '|#|)
      (list tape)
      (if (assoc label *abbreviations*)
	  (let ((results '()))
	    (dolist (word (cdr (assoc label *abbreviations*)) results)
	      (setq results (cons (append tape (list word)) results))))
	  (list (append tape (list label))))))

(defun generate-next (node tape network)
  "Prints out sentences"
  (if (member node (final-nodes network))
      (print tape)
      (dolist (transition (transitions network))
	(if (equal node (trans-node transition))
	    (dolist (newtape (generate-move (trans-label transition) tape))
	      (generate-next (trans-newnode transition) newtape network))
	    '())))) ; Transition from the wrong node

(defun generate (network)
  "Generates valid network sentences of the given network."
  (dolist (initial-node (initial-nodes network))
    (generate-next initial-node Nil network))
  T)
