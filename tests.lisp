(ql:quickload :bld-linalg)
(ql:quickload :fiveam)

(in-package :bld-linalg)

(use-package :fiveam)

(def-suite :bld-linalg)

(in-suite :bld-linalg)

(test eye
  (is (= 2 (array-rank (eye 1)) (array-rank (eye 2)) (array-rank (eye 3))))
  (is-true (equalp #2a((1 0 0)(0 1 0)(0 0 1)) (eye 3))))

(test normf
  (is (= (normf #2a((1 2 3)(-4 -5 -6))) (sqrt (+ 1 4 9 16 25 36)))))

(test transpose
  (is-true (equalp (transpose #2a((1 2 3)(4 5 6)))
		   #2a((1 4)(2 5)(3 6)))))

(test +
  (is-true (equalp (+ #2a((1 2 3)(4 5 6)) #2a((7 8 9)(10 11 12)))
		   #2a((8 10 12)(14 16 18)))))

(test *
  (is-true (equalp (* #2a((1 2 3)(4 5 6)) #2a((7 8)(9 10)(11 12)))
		   #2a((58 64)(139 154))))
  (is-true (equalp (* #2a((1 2 3)(4 5 6)) 2 3)
		   #2a((6 12 18)(24 30 36))))
  (is-true (equalp (* 2 3 #2a((1 2 3)(4 5 6)))
		   #2a((6 12 18)(24 30 36)))))

(test copya
  (is-true (equalp (copya #2a((1 2 3)(4 5 6))) #2a((1 2 3)(4 5 6))))
  (is-true (let* ((a #2a((1 2 3)(4 5 6)))
		  (b (copya a)))
	     (setf (aref b 0 0) 2)
	     (and (equalp a #2a((1 2 3)(4 5 6)))
		  (equalp b #2a((2 2 3)(4 5 6)))))))

(test diag
  (is-true (every #'= #(1 2 3) (diag #2a((1 0 0)(0 2 0)(0 0 3))))))

(test column-vector
  (is-true (equalp (column-vector 1 2 3) #2a((1)(2)(3)))))

(test row-vector
  (is-true (equalp (row-vector 1 2 3) #2a((1 2 3)))))

(test subm
  (is-true (equalp (subm #2a((1 2 3)(4 5 6)(7 8 9)) '(1 2) '(1 2))
		   #2a((5 6)(8 9)))))

(test .*
  (is-true (equalp (.* #2a((2 3)(4 5)) #2a((3 4)(5 6)) #2a((4 5)(6 7)))
                   #2a((24 60)(120 210))))
  (is-true (equalp (.* #2a((2 3)(4 5))) #2a((2 3)(4 5)))))
