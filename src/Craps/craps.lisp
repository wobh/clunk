;;;; -*- Mode:lisp;coding:utf-8 -*-
;;;; FILE: craps.lisp

;;;; DESCRIPTION

;;;; Common Lisp craps

;;;; AUTHORS

;;;; William H. Clifford <wobh@wobh.org>

;;;; NOTES

(defpackage "ORG.WOBH.CRAPS"
  (:nicknames "CRAPS")
  (:use "COMMON-LISP")
  (:export Nil)
  (:documentation ""))

(in-package "ORG.WOBH.CRAPS")

(defun shoot-craps-die ()
  (1+ (random 6)))

(defun valid-craps-die-roll-p (shot)
  (typep shot '(integer 1 6)))

(defun shoot-craps-dice ()
  (list (shoot-craps-die) (shoot-craps-die)))

(defun crap-on-invalid-craps-shot (craps-shot)
  (assert (= (length craps-shot) 2))
  (assert (every #'valid-die-shot craps-shot)))

(defun eval-craps-shot (craps-shot)
  (crap-on-invalid-craps-shot (craps-shot))
  (maplist #'+ craps-shot))

(defun craps-shot-index (craps-shot)
  (crap-on-invalid-craps-shot (craps-shot))
  (apply #'+
	 (maplist (lambda (a b)
		    (* (car x) (apply #'* (cdr y))))
		  (maplist #'1- craps-shot) '(6 6))))

(defparameter *craps-shot-names*
  '(snake-eyes ace-deuce  easy-four  fever-five easy-six   seven-out
    ace-deuce  hard-four  fever-five easy-six   seven-out  easy-eight
    easy-four  fever-five hard-six   seven-out  easy-eight nina-nine
    fever-five easy-six   seven-out  hard-eight nina-nine  easy-ten
    easy-six   seven-out  easy-eight nina-nine  hard-ten   yo-leven
    seven-out  easy-eight nina-nine  easy-ten   yo-leven   box-cars))

(defun get-craps-shot-name (craps-shot)
  (elt *craps-shot-names* (craps-shot-index craps-shot)))

;;;; TODO: betting