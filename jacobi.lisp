(in-package :bld-linalg)

(defun normoffdiag (a)
  "Norm of off-diagonal elements"
  (loop with (m n) = (array-dimensions a)
     for i below n
     sum (loop for j below n
	    when (/= i j)
	    sum (expt (aref a i j) 2))))

(defun symschur2 (a p q)
  (if (not (zerop (aref a p q)))
      (let* ((tau (/ (- (aref a q q) (aref a p p)) (* 2 (aref a p q))))
	     (tan (if (>= tau 0)
		     (/ (+ tau (sqrt (1+ (expt tau 2)))))
		     (/ (- tau (sqrt (1+ (expt tau 2)))))))
	     (c (/ (sqrt (1+ (expt tan 2)))))
	     (s (* tan c)))
	(list c s))
      (list 1 0)))

(defun j (n p q c s)
  (let ((m (eye n)))
    (setf (aref m p p) c)
    (setf (aref m q q) c)
    (setf (aref m p q) s)
    (setf (aref m q p) (- s))
    m))

(defun jacobi (ain tol)
  (destructuring-bind (m n) (array-dimensions ain)
    (let ((a (copya ain))
	  (v (eye n))
	  (eps (* tol (normf ain))))
      (loop while (> (normoffdiag a) eps)
	 do (loop for p from 0 below (- n 1)
	       do (loop for q from (1+ p) below n
		     for (c s) = (symschur2 a p q)
		     for j = (j n p q c s)
		     do (setq a (* (* (transpose j) a) j))
		     do (setq v (* v j)))))
      (values a v))))
