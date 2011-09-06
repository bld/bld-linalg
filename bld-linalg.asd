(asdf:defsystem :bld-linalg
  :description "Linear algebra with 2D arrays"
  :author "Ben Diedrich <bldiedrich@gmail.com>"
  :license "LLGPL"
  :components ((:file "package")
	       (:file "linalg" :depends-on ("package"))
	       (:file "jacobi" :depends-on ("linalg"))))
