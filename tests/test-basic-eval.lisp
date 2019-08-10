(in-package :scheme-compiler-tests)


(define-test test-basic-eval
  (assert-equal (schemeval
		 4
		 *the-global-environment*) 4)
  (assert-equal (schemeval
		 '(define (f x) (* x 2))
		 *the-global-environment*)
		*define-return*))

(define-test test-definitions
  (assert-equal (schemeval
		 '(define (square x) (* x x))
		 *the-global-environment*)
		*define-return*)
  (assert-equal (schemeval '(* 2 2)
			   *the-global-environment*)
		4)
  (assert-equal (schemeval '(square 3)
			   *the-global-environment*)
		9))

(define-test test-long-body
  (assert-equal (schemeval
		 '(define (longbody x y z) x y z)
		 *the-global-environment*)
		*define-return*)
  (assert-equal (schemeval '(longbody 1 2 3)
			   *the-global-environment*)
		3))


