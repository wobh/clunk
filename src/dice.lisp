;;; FIXME: roll-die and roll-n-dice should be more generic
  
(defun roll-die (d &optional (natural-die nil))
  (if natural-die (1+ (random d)) (random d)))

(defun roll-n-dice (d n &optional (natural-die nil))
  (labels ((roll-n-dice-r (rolls d n &optional (natural-die nil))
             (cond ((zerop n) rolls)
                   (t
                    (roll-n-dice-r 
                     (cons (roll-die d natural-die) rolls)
                     d (1- n) natural-die)))))
    (roll-n-dice-r () d n natural-die)))
  
(defun roll-n-dice-sum (d n &optional (natural-die nil))
  (reduce #'+ (roll-n-dice d n natural-die)))
  
(defun roll-n-dice-average (d n &optional (natural-die nil))
  (let ((rolls (roll-n-dice d n natural-die)))
    (/ (reduce #'+ rolls) (length rolls))))
