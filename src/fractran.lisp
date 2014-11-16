;;;; fractran.lisp

;;;; AUTHORS:

;;;; William Clifford [wc] wobh@yahoo.com

;;;; DESCRIPTION

;;;; A package implementing John Conway's FRACTRAN programming language.

;;;; http://en.wikipedia.org/wiki/FRACTRAN
;;;; http://mathworld.wolfram.com/FRACTRAN.html
;;;; http://esolangs.org/wiki/Fractran
;;;; http://brainwagon.org/2006/10/27/prime-number-pathology-courtesy-of-good-math-bad-math/

(in-package #:cl)

(defpackage #:fractran
  (:use #:cl)
  (:export #:defractor)
  (:export #:+conway-primegame+ #:+conway-pigame+ #:+conway-polygame)
  (:documentation "``To play the fraction game corresponding to a given list

f_1, f_2, ... ,f_k

of fractions and starting integer N, you repeatedly multiply the
integer you have at any stage (initially N) by the earliest f_i in the
list for which the answer is integral. Whenever there is no such f_i,
the game stops.''

Conway, J.H., _FRACTRAN: A Simple Universal Programming Language for
Arithmetic_, _Open Problems in Communication and Computation_, 1987,
pp 4-26"))

(defpackage #:fractran-tests
  (:use #:cl #:fractran))

(in-package #:fractran-tests)

(defconstant +conway-primegame-specs+
  (list 15 825 725 1925 2275 425 390 330 290 770 910 
	170 156 132 116 308 364 68 4 30 225 12375 10875 
	28875 25375 67375 79625 14875 13650 2550 2340 
	1980 1740 4620 4060 10780 12740 2380 2184 408 152)
  "http://oeis.org/A007542")

(defun conway-primegame-test ()
  (let ((pg (defractor 2 +conway-primegame+)))
    (loop
       for index from 0 below (length +conway-primegame-specs+)
       do
	 (let ((result (funcall pg))
	       (expect (nth index +conway-primegame-specs+)))
	   (assert (= result expect)
		   (index result expect)
		   "PRIMEGAME: Mismatch at index ~A. Output, ~A. Expected ~A"
		   index result expect)))))

(in-package #:fractran)

(defun defractor (input progf)
  "Returns a lambda which outputs the next step in FRACTRAN computation."
  ;; (check-type input (integer (0)))
  ;; (assert (every (lambda (n) (typep n '(rational (0)))) progf))
  (lambda ()
    (when input
      (let ((n (find-if 'integerp progf :key (lambda (f) (* input f)))))
	(setf input (when n (* n input)))))))

(defconstant +conway-primegame+
  (list 17/91 78/85 19/51 23/38 29/33 77/29 95/23
	77/19 1/17 11/13 13/11 15/14 15/2 55/1)
  "``When PRIMEGAME is started at 2 the other powers of 2 that appear, namely,

2^2, 2^3, 2^5, 2^7, 2^ 11, 2^13, 2^17, 2^19, 2^23, 2^29, ... ,

are precisely those whose indicies are the prime numbers in order of
magnitude.''

J.H. Conway, _FRACTRAN: A Simple Universal Programming Language for
Arithmetic_, _Open Problems In Communication And Computation_, 1987,
pp 4-26")

(defconstant +conway-pigame+
  (list 365/46 29/161 79/575 679/451 3159/413 83/407 473/371 638/355 434/335 89/235 17/209 79/122
	31/183 41/115 517/89 111/83 305/79 23/73 73/71 61/67 37/61 19/59 89/57 41/53 833/47 53/43
	86/41 13/38 23/37 67/31 71/29 83/19 475/17 59/13 41/291 1/7 1/11 1/1024 1/97 89/1)
  "``When PIGAME is started at 2^n, the next power of 2 to appear is
2^pi(n), where for

n = 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 ...

pi(n) = 3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3 2 3 8 4 6 ...

For an arbitrary natural number n, pi(n) is the nth digit after the
point in the decimal expansion of of the number pi.''

J.H. Conway, _FRACTRAN: A Simple Universal Programming Language for
Arithmetic_, _Open Problems In Communication And Computation_, 1987,
pp 4-26")

(defconstant +conway-polygame+
  (list 583/559 629/551 437/527 82/517 615/329 371/129 1/115 53/86 43/53 23/47 341/46
	41/43 47/41 29/37 37/31 37/31 299/29 47/23 161/15 527/19 159/7 1/17 1/13 1/3)
  "``Define f_c(n) = m if POLYGAME when started at c^2^2^n, stops at 2^2^m,
and otherwise leave f_c(n) undefined. Then every computable function
appears among f_0, f_1, f_2,... .''

J.H. Conway, _FRACTRAN: A Simple Universal Programming Language for
Arithmetic_, _Open Problems In Communication And Computation_, 1987,
pp 4-26")






