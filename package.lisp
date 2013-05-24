(defpackage bld-linalg
  (:use :cl)
  (:import-from bld-gen defmeth1 defmeth2 defmeth12)
  (:shadowing-import-from bld-gen
			  + - * / expt
			  sin cos tan
			  atan asin acos
			  sinh cosh tanh
			  asinh acosh atanh
			  log exp sqrt abs
			  min max signum)
  (:export eye normf transpose + * copya jacobi diag column-vector row-vector subm setsubm))
