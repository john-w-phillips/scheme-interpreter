(in-package :scheme-compiler-tests)

(define-test test-apply
  (is
   equal
   6
   (schemeval { (apply + '(1 2 3)) }
	      *the-global-environment*))
  (is
   equal
   6
   (schemeval { (apply + 1 2 3) }
	      *the-global-environment*)))
(define-test test-interpreter-eval
  (is
   equal
   6
   (schemeval
    { (eval '(+ 1 2 3) user-initial-environment) }
    *the-global-environment*)))
