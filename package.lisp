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
  (:export eye normf transpose + * copya jacobi get-diag diag make-v-diag make-diag column-vector row-vector subm setsubm mapm for-subs-row back-subs-row outer-prod-gauss-elim inverse solve row-vector-to-list col-vec-to-list 2dvector-to-list conjugate-transpose hermetianp rows columns cat-row cat-col zeros))
