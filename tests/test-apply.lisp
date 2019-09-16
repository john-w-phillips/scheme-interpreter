(in-package :scheme-compiler-tests)

(define-test test-apply
  (assert-equal
   6
   (schemeval { (apply + '(1 2 3)) }
	      *the-global-environment*))
  (assert-equal
   6
   (schemeval { (apply + 1 2 3) }
	      *the-global-environment*)))
(define-test test-interpreter-eval
  (assert-equal
   6
   (schemeval
    { (eval '(+ 1 2 3) user-initial-environment) }
    *the-global-environment*)))




