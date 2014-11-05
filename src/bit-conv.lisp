
(defparameter *power-table*
  '(( 0 .     bit)
    ( 3 .     byte)
    (10 . kilobit)
    (13 . kilobyte)
    (20 . megabit)
    (23 . megabyte)
    (30 . gigabit)
    (33 . gigabyte)
    (40 . terabit)
    (43 . terabyte)
    (50 . petabit)
    (53 . petabyte)
    (60 . exabit)
    (63 . exabyte)
    (70 . zettabit)
    (73 . zettabyte)
    (80 . yottabit)
    (83 . yottabyte)))

(defun get-power (name &optional (table *power-table*))
  (first (rassoc name table)))

(defun calc-power (power-name)
  (expt 2 (get-power power-name)))

(defun bit-conv (amount power-name power-wanted)
  (/ (* amount (calc-power power-name))
     (calc-power power-wanted)))