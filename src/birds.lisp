;;;; Combinatory birds

(defun s (f g x)
  (f x (g x)))

(defun k (x y)
  x)

(defun i (x)
  (s (k (k x))))

(defun mockp (b0 b1 b2)
  "With b0, does b1 mock b2"
  (equal (b1 b0) (b2 b0)))

(defun composep (b0 b1 b2 b3)
  "With b0, does b1 compose b2 and b3?"
  (equal (b1 b0) (b3 (b2 b0))))

(defun narcissistp (b0 b1)
  "With b0, does b1 think of itself?"
  (equal b1 (b1 b0)))