(asdf:defsystem :bld-linalg
  :description "Linear algebra with 2D arrays"
  :author "Ben Diedrich <bldiedrich@gmail.com>"
  :license "Mozilla Public License"
  :depends-on ("bld-gen")
  :serial t
  :components ((:file "package")
	       (:file "linalg")
	       (:file "jacobi")
	       (:file "lu")
	       (:file "gj")))
