(in-package :scheme-compiler-tests)

(define-test test-and
  (assert-true
   (schemeval { (and true true true) } *the-global-environment*))
  (assert-equal
   *scheme-false-value*
   (schemeval { (and true true true false) } *the-global-environment*)))
