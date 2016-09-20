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
  (let* ((dims (array-dimensions a))
	 (c (make-array dims :initial-element 0)))
    (dotimes (i (apply #'* dims))
      (setf (row-major-aref c i) (+ (row-major-aref a i) (row-major-aref b i))))
    c))

(defmeth2 * ((a array) (s t))
  (let* ((dims (array-dimensions a))
	 (b (make-array dims :initial-element 0)))
      (dotimes (i (apply #'* dims))
	(setf (row-major-aref b i) (* (row-major-aref a i) s)))
      b))

(defmeth2 * ((s t) (a array))
  (* a s))

(defmeth2 * ((a array) (b array))
  (destructuring-bind (m p) (array-dimensions a)
    (destructuring-bind (p n) (array-dimensions b)
      (let ((c (make-array (list m n) :initial-element 0)))
	(dotimes (i m)
	  (dotimes (j n)
	    (setf (aref c i j) 
		  (loop with summation = 0
		     for k below p 
		     do (setq summation (+ summation (* (aref a i k) (aref b k j))))
		       finally (return summation)))))
	c))))

(defmeth2 / ((a array) (s t))
  (* a (/ s)))

(defun copya (a)
  "Copy 2D array"
  (let* ((dim (array-dimensions a))
	 (acpy (make-array dim :initial-element 0)))
    (dotimes (i (reduce #'* dim))
      (setf (row-major-aref acpy i) (row-major-aref a i)))
    acpy))

(defun get-diag (a)
  "Pull out the diagonal of an array into a 1D vector"
  (destructuring-bind (m n) (array-dimensions a)
    (let ((v (make-array (min m n) :initial-element 0)))
      (loop for i below (min m n)
	 do (setf (aref v i) (aref a i i)))
      v)))

(defun diag (a)
  (get-diag a))

(defun make-diag (&rest args)
  "Create a diagonal array from arguments"
  (let* ((n (length args))
	 (m (make-array (list n n))))
    (loop for a in args
       for i below n
       do (setf (aref m i i) a))
    m))

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

(defun conjugate-transpose (m)
  "Conjugate transpose of a matrix (2D array)"
  (let* ((dims (array-dimensions m))
	 (mc (make-array (reverse dims))))
    (dotimes (i (second dims))
      (dotimes (j (first dims))
	(setf (aref mc i j) (conjugate (aref m j i)))))
    mc))

(defun hermetianp (m)
  (equalp m (conjugate-transpose m)))

(defun rows (m)
  (array-dimension m 0))

(defun columns (m)
  (array-dimension m 1))

(defun cat-row (a b)
  "Concatenate 2D arrays by row"
  (destructuring-bind (row-a col-a) (array-dimensions a)
    (destructuring-bind (row-b col-b) (array-dimensions b)
      (assert (= col-a col-b))
      (let ((c (make-array (list (+ row-a row-b) col-a)))
       	    (size-a (* row-a col-a))
	    (size-b (* row-b col-b)))
      	(dotimes (i (+ size-a size-b))
      	  (setf (row-major-aref c i)
      		(if (< i size-a)
      		    (row-major-aref a i)
      		    (row-major-aref b (- i size-a)))))
      	c))))

(defun cat-col (a b)
  "Concatenate 2D arrays by column"
  (transpose (cat-row (transpose a) (transpose b))))

