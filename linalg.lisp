(in-package :bld-linalg)

(defun eye (n)
  "Identity matrix"
  (let ((m (make-array (list n n) :initial-element 0)))
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
  "Transpose a matrix"
  (destructuring-bind (m n) (array-dimensions a)
    (let ((b (make-array (list n m) :initial-element 0)))
      (dotimes (i m)
	(dotimes (j n)
	  (setf (aref b j i) (aref a i j))))
      b)))

(defmeth2 + ((a array) (b array))
  (assert (equal (array-dimensions a) (array-dimensions b)))
  (destructuring-bind (m n) (array-dimensions a)
    (let ((c (make-array (list m n) :initial-element 0)))
      (dotimes (i (* m n))
	(setf (row-major-aref c i) (+ (row-major-aref a i) (row-major-aref b i))))
      c)))
  
(defmeth2 * ((a array) (s number))
  (destructuring-bind (m n) (array-dimensions a)
    (let ((b (make-array (list m n) :initial-element 0)))
      (dotimes (i (* m n))
	(setf (row-major-aref b i) (* (row-major-aref a i) s)))
      b)))
(defmeth2 * ((s number) (a array))
  (* a s))

(defmeth2 * ((a array) (b array))
  (destructuring-bind (m p) (array-dimensions a)
    (destructuring-bind (p n) (array-dimensions b)
      (let ((c (make-array (list m n) :initial-element 0)))
	(dotimes (i m)
	  (dotimes (j n)
	    (setf (aref c i j) 
		  (loop for k below p 
		     sum (* (aref a i k) (aref b k j))))))
	c))))

(defun copya (a)
  "Copy 2D array"
  (let* ((dim (array-dimensions a))
	 (acpy (make-array dim :initial-element 0)))
    (dotimes (i (reduce #'* dim))
      (setf (row-major-aref acpy i) (row-major-aref a i)))
    acpy))

(defun diag (a)
  "Pull out the diagonal of an array into a 1D vector"
  (destructuring-bind (m n) (array-dimensions a)
    (let ((v (make-array (min m n) :initial-element 0)))
      (loop for i below (min m n)
	 do (setf (aref v i) (aref a i i)))
      v)))

(defun column-vector (&rest args)
  "Create a column vector as 2D array"
  (make-array (list (length args) 1) :initial-contents (mapcar #'list args)))

(defun row-vector (&rest args)
  "Create a row vector as 2D array"
  (make-array (list 1 (length args)) :initial-contents (list args)))

(defun subm (a rows cols)
  "Sub matrix given list of rows and columns"
  (make-array 
   (list (length rows) (length cols))
   :initial-contents
   (loop for row in rows
      collect (loop for col in cols
		 collect (aref a row col)))))

(defun setsubm (a rows cols b)
  "Set the specified rows and columns of matrix A to those of B"
  (loop for row in rows
     for rowi = 0 then (incf rowi)
     do (loop for col in cols
	   for coli = 0 then (incf coli)
	     do (setf (aref a row col) (aref b rowi coli))))
  a)

(defun mapm (fn &rest ms)
  "Map a function across the elements of matrices of the same dimension"
  (destructuring-bind (r c) (array-dimensions (first ms))
    (let ((mout (make-array (list r c) :initial-element 0)))
      (dotimes (ri r)
	(dotimes (ci c)
	  (setf (aref mout ri ci) 
		(apply fn (mapcar #'(lambda (m) (aref m ri ci)) ms)))))
      mout)))
    
(defmeth12 - ((a array) (b array))
  ((mapm #'- a))
  ((mapm #'- a b)))

(defun row-vector-to-list (v)
  "Turn a 2D array row vector into a list"
  (loop for i below (second (array-dimensions v))
     collect (aref v 0 i)))

(defun col-vec-to-list (v)
  "Turn a 2D array column vector into a list"
  (loop for i below (first (array-dimensions v))
     collect (aref v i 0)))

(defun 2dvector-to-list (v)
  "Turn a 2D array row or column vector into a list"
  (let ((dim (array-dimensions v)))
    (cond
      ((= (first dim) 1) (row-vector-to-list v))
      ((= (second dim) 1) (col-vec-to-list v))
      (t (warn "Not a 2d vector") nil))))
