;;;; madlibs


(format nil
	"That ~a, ~a sure is a ~a ~a"
	person-first-name
	person-pronoun
	adjective
	noun)

(defun remove-nth (index seq)
  (remove-if (constantly t) seq :start index :end (1+ index)))

(defun pb-randomize-sequence (sequence)
   (loop
      :with vector = (coerce sequence 'vector)
      :for i :from (1- (length vector)) :downto 1
      :do (rotatef (aref vector i) (aref vector (random i)))
      :finally (return (coerce vector (type-of sequence)))))

(defun jm-shuffle (list)
  (if (or (null list)
          (null (cdr list)))
      list
      (do ((tail list (cdr tail))
	   (odd  '()  (cons (car tail) even))
	   (even '()  odd))
	  ((null tail)
	   (merge 'list
		  (jm-shuffle odd)
		  (jm-shuffle even)
		  (lambda (a b)
		    (zerop (random 2))))))))

(defun wc-randomize-list (seq)
  (wc-randomize-list-r seq ()))
  
(defun wc-randomize-list-r (src-seq dst-seq)
  "Recursively randomize a list"
  (cond ((= 1 (length src-seq)) (cons (car src-seq) dst-seq))
	(t
	 (let ((r (elt src-seq (random (length src-seq)))))
	   (wc-randomize-list-r
	    ((cdr src-seq)
	    (cons (car src-seq) dst-seq))))))

