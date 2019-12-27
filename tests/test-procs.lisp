(in-package :scheme-compiler-tests)

(define-test test-lambdas
  (true (lambda? { (lambda (x y z) (cons x (cons y (cons z '())))) }))
  (is equalp
      { (x y z) }
      (lambda-vars { (lambda (x y z) (+ x (+ y z))) }))
  (is equalp
      { ((+ x (+ y z))
	 (* x y)) }
      (lambda-body { (lambda (x y z) (+ x (+ y z)) (* x y)) })))
	 

      
