;;; http://web.mat.bham.ac.uk/atlas/v2.0/info/blackbox.html
;;; GAP http://www.gap-system.org/gap.html

;;; monster group

;; Order = 808,017,424,794,512,875,886,459,904,961,710,757,005,754,368,000,000,000
;;       = 2^46 * 3^20 * 5^9 * 7^6 * 11^2 * 13^3 * 17 * 19 * 23 * 29 * 31 * 41 * 47 * 59 * 71
;; Mult  = 1
;; Out   = 1

;; # Black box algorithm to find standard generators of the Fischer-Griess
;; # Monster group M.

;; 	set F 0
;; 	set G 0
;; 	set V 0
;; lbl SEMISTD
;; 	rand 1
;; 	ord 1 A
;; 	incr V
;; 	if V gt 1000 then timeout
;; 	if A notin 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 &
;; 			20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 &
;; 			35 36 38 39 40 41 42 44 45 46 47 48 50 51 52 &
;; 			54 55 56 57 59 60 62 66 68 69 70 71 78 84 87 &
;; 			88 92 93 94 95 104 105 110 119 then fail
;; 	if F eq 0 then
;; 		if A in 34 38 50 54 62 68 94 104 110 then
;; 			div A 2 B
;; 			pwr B 1 2
;; 			set F 1
;; 		endif
;; 	endif
;; 	if G eq 0 then
;; 		if A in 9 18 27 36 45 54 then
;; 			div A 3 C
;; 			pwr C 1 3
;; 			set G 1
;; 		endif
;; 	endif

;; 	if F eq 0 then jmp SEMISTD
;; 	if G eq 0 then jmp SEMISTD
	
;; 	set X 0
;; lbl CONJUGATE
;; 	incr X
;; 	if X gt 1000 then timeout
;; 	rand 4
;; 	cjr 3 4
;; 	mu 2 3 5
;; 	ord 5 D
;; 	if D notin 3 6 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 &
;; 			23 24 25 26 27 28 29 30 31 32 33 34 35 36 &
;; 			38 39 40 41 42 45 46 48 50 51 55 56 60 62 &
;; 			66 68 69 70 71 78 84 88 94 104 105 then fail
;; 	if D noteq 29 then jmp CONJUGATE

;; 	oup 2 2 3

(defclass group ()
  ((elements :accessor elements :initval (make))))

(defmethod mu ((group group) e1 e2 e3)
  (with-slots (elements) group
    (setf (elt elements e3))))

(let ((f 0)
      (g 0)
      (v 0)
      (a nil)
      (x nil)
      (d nil))
  (tagbody
   semistd
     ()
     ()
     (incf v)
     (when (< 1000 v) (error "timeout"))
     (unless (member a '( 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
                         16 17 18 19 20 21 22 23 24 25 26 27 28 29 30
                         31 32 33 34 35 36 38 39 40 41 42 44 45 46 47
                         48 50 51 52 54 55 56 57 59 60 62 66 68 69 70
                         71 78 84 87 88 92 93 94 95   104 105 110 119))
       (error "fail"))
     (when (zerop f)
       (when (member a '(34 38 50 54 62 68 94 104 110))
         ()
         ()
         (setf f 1)))
     (when (zerop g)
       (when (member a '(9 18 2736 45 54))
         ()
         ()
         (setf g 1)))
     (when (= f 0) (go semistd))
     (when (= g 0) (go semistd))
     (setf x 0)
   conjugate
     (incf x)
     (when (< 1000 x) (error "timeout"))
     ()
     ()
     ()
     ()
     (unless (member d '( 3  6  8  9 10 11 12 13 14 15 16 17  18  19
                         20 21 22 23 24 25 26 27 28 29 30 31  32  33
                         34 35 36 38 39 40 41 42 45 46 48 50  51  55
                         56 60 62 66 68 69 70 71 78 84 88 94 104 105))
       (error "fail"))
     (unless (= d 29)
       (go conjugate))
     ()
     ))



;; # Checker for Monster group M

;; # Check orders from definition
;; chor 1 2
;; chor 2 3
;; mu 1 2 3
;; chor 3 29

;; # Find a 2A element d
;; mu 3 2 4
;; pwr 4 3 5
;; mu 4 4 6
;; mu 5 6 7
;; chor 7 50
;; pwr 25 7 8

;; # Check the order of ad; it should be 5,
;; # proving that a is a 2A element
;; mu 1 8 9
;; chor 9 5

;; # Find the order of ab^f for some f; it should be 34,
;; # proving that b is a 3B element.
;; mu 5 4 10
;; iv 10 11
;; mu 1 11 12
;; mu 12 2 13
;; mu 13 10 14
;; chor 14 34

(defun notrel (rel)
  (let* ((rels '("eq" "in" "gt" "lt" "geq" "leq" "notin" "noteq"))
         (i (position rel rels)))
    (if (null i)
        (error "unknown relation ~S" rel)
        (- 9 i))))

(defun prepare-blackbox (filename)
  (let ((bb-keywords '("add" "break" "call" "chcl" "chor" "cj" "cjr" "com"
                     "cp" "decr" "div" "echo" "else" "elseif" "endif"
                     "fail" "false" "if" "incr" "inv" "iv"
                     "jmp" "lbl" "mod" 
                     "mu" "mul" "nop" "ord" "oup" "pwr" "rand" "return"
                     "set" "sub" "timeout" "true"))
        (bb-labels   '())
        (bb-prog     '())
        (bb-linenums '()))
    ))

(defun perform-instruction (fullline, ins, g, ans, gpelts, ctr, options))
