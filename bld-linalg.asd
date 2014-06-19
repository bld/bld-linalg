(asdf:defsystem :bld-linalg
  :description "Linear algebra with 2D arrays"
  :author "Ben Diedrich <bldiedrich@gmail.com>"
  :license "MIT"
  :depends-on ("bld-gen")
  :serial t
  :components ((:file "package")
	       (:file "linalg")
	       (:file "jacobi")
	       (:file "lu")))
