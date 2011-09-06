(in-package :bld-linalg)

(defun eye (n)
  "Identity matrix"
  (let ((m (make-array (list n n))))
    (dotimes (i n)
      (setf (aref m i i) 1))
    m))

(defun normf (a)
  "Frobenius norm"
  (sqrt
   (loop with (m n) = (array-dimensions a)
      for i below m
      sum (loop for j below n
	     sum (expt (abs (aref a i j)) 2)))))

(defun transpose (a)
  (destructuring-bind (m n) (array-dimensions a)
    (let ((b (make-array (list n m))))
      (dotimes (i m)
	(dotimes (j n)
	  (setf (aref b j i) (aref a i j))))
      b)))

(defun m+2 (a b)
  (destructuring-bind (m n) (array-dimensions a)
    (let ((c (make-array (list m n))))
      (dotimes (i (* m n))
	(setf (row-major-aref c i) (+ (row-major-aref a i) (row-major-aref b i))))
      c)))

(defun m*s (a s)
  (destructuring-bind (m n) (array-dimensions a)
    (let ((b (make-array (list m n))))
      (dotimes (i (* m n))
	(setf (row-major-aref b i) (* (row-major-aref a i) s)))
      b)))

(defun m*2 (a b)
  (destructuring-bind (m p) (array-dimensions a)
    (destructuring-bind (p n) (array-dimensions b)
      (let ((c (make-array (list m n))))
	(dotimes (i m)
	  (dotimes (j n)
	    (setf (aref c i j) 
		  (loop for k below p 
		     sum (* (aref a i k) (aref b k j))))))
	c))))

(defun copya (a)
  (let* ((dim (array-dimensions a))
	 (acpy (make-array dim)))
    (dotimes (i (reduce #'* dim))
      (setf (row-major-aref acpy i) (row-major-aref a i)))
    acpy))
