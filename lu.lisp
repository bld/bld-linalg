;;; LU factorization & matrix inverses

(in-package :bld-linalg)

(defun for-subs-row (l bin &optional (n (apply #'max (array-dimensions bin))))
  "Forward substitution by row to solve Lx=bin for x"
  (let ((b (copya bin)))
    (setf (aref b 0 0) (/ (aref b 0 0) (aref l 0 0)))
    (loop for i from 1 below n
       do (setf (aref b i 0)
		(/ (- (aref b i 0)
		      (loop for j below i
			 sum (* (aref l i j) (aref b j 0))))
		   (aref l i i))))
    b))

(defun back-subs-row (u bin &optional (n (apply #'max (array-dimensions bin))))
  "Backward substitution by row to solve Ux=bin for x"
  (let ((b (copya bin)))
    (setf (aref b (1- n) 0) (/ (aref b (1- n) 0) (aref u (1- n) (1- n))))
    (loop for i from (- n 2) downto 0
       do (setf (aref b i 0)
		(/ (- (aref b i 0)
		      (loop for j from (1+ i) below n
			 sum (* (aref u i j) (aref b j 0))))
		   (aref u i i))))
    b))

(defun outer-prod-gauss-elim (ain &optional (n (apply #'max (array-dimensions ain))))
  "Outer product Gaussian elimination of array Ain, returning A, L, & U"
  (let ((a (copya ain))
	(l (make-array (list n n)))
	(u (make-array (list n n))))
    (loop for k below (1- n)
       for rows = (loop for ri from (1+ k) below n collect ri)
       do (dolist (ri rows)
	    (setf (aref a ri k) (/ (aref a ri k) (aref a k k))))
       do (setsubm a rows rows (- (subm a rows rows) 
				  (* (subm a rows (list k))
				     (subm a (list k) rows)))))
    (dotimes (ri n)
      (dotimes (ci n)
	(cond
	  ((= ri ci) ; diagonals of l & u
	   (setf (aref l ri ci) 1) ; l is ones
	   (setf (aref u ri ci) (aref a ri ci)))
	  ((> ri ci) ; lower triangle of l
	   (setf (aref l ri ci) (aref a ri ci)))
	  ((> ci ri) ; upper triangle of u
	   (setf (aref u ri ci) (aref a ri ci))))))
    (values a l u)))

(defun inverse (a &optional (n (apply #'min (array-dimensions a))))
  "Inverse of square 2D array A"
  (assert (apply #'= (array-dimensions a)))
  (let ((ainv (make-array (list n n)))
	(b (eye n))
	(rows (loop for i below n collect i)))
    (multiple-value-bind (ared l u) (outer-prod-gauss-elim a n)
      (loop for j below n
	 for bi = (subm b rows (list j))
	 for yi = (for-subs-row l bi)
	 for xi = (back-subs-row u yi)
	 do (dotimes (i n)
	      (setf (aref ainv i j) (aref xi i 0)))))
    ainv))
