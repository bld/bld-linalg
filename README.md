bld-linalg
==========

Basic linear algebra functions for Common Lisp

Why? 
----

Because I only needed a few simple operations and didn't want the
overhead of a full linear algebra library.

What's included?
----------------

* Treats 2D arrays as matrices

* Overloaded arithmetic functions (using BLD-GEN) to implement
  addition, subtraction, and multiplication of matrices

* Basic matrix processing like identity, diagonal, column & row
  vectors, sub-matrices, and matrix mapping

* Frobenius norm

* Jacobi method

* LU decomposition
