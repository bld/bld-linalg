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

(defmeth2 + ((a array) (b array))
  (assert (equal (array-dimensions a) (array-dimensions b)))
  (destructuring-bind (m n) (array-dimensions a)
    (let ((c (make-array (list m n))))
      (dotimes (i (* m n))
	(setf (row-major-aref c i) (+ (row-major-aref a i) (row-major-aref b i))))
      c)))
  
(defmeth2 * ((a array) (s number))
  (destructuring-bind (m n) (array-dimensions a)
    (let ((b (make-array (list m n))))
      (dotimes (i (* m n))
	(setf (row-major-aref b i) (* (row-major-aref a i) s)))
      b)))
(defmeth2 * ((s number) (a array))
  (* a s))

(defmeth2 * ((a array) (b array))
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

(defun diag (a)
  (destructuring-bind (m n) (array-dimensions a)
    (let ((v (make-array (min m n))))
      (loop for i below (min m n)
	 do (setf (aref v i) (aref a i i)))
      v)))

(defun column-vector (&rest args)
  (make-array (list (length args) 1) :initial-contents (mapcar #'list args)))

(defun row-vector (&rest args)
  (make-array (list 1 (length args)) :initial-contents (list args)))

(defun subm (a rows cols)
  (make-array 
   (list (length rows) (length cols))
   :initial-contents
   (loop for row in rows
      collect (loop for col in cols
		 collect (aref a row col)))))

(defun setsubm (a rows cols b)
  (loop for row in rows
     for rowi = 0 then (incf rowi)
     do (loop for col in cols
	   for coli = 0 then (incf coli)
	     do (setf (aref a row col) (aref b rowi coli))))
  a)
