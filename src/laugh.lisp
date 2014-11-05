;;;;

(defun make-laugh (&optional (n 1))
  (apply #'concatenate 'string
	 (append (make-list n :initial-element "ha")
		 '("!"))))
(loop
   with h = #\h
   with a = #\a
   with ! = #\!
   with l = 5
   with s = (make-string 0)
   for n from 0 to (1- l)
     initially (
   do
     (
