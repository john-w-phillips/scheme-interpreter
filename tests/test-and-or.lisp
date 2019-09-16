(in-package :scheme-compiler-tests)

(define-test test-and
  (assert-true
   (schemeval { (and true true true) } *the-global-environment*))
  (assert-true
   (schemeval { (and) } *the-global-environment*))
  (assert-equal
   *scheme-false-value*
   (schemeval { (and true true true false) } *the-global-environment*)))

(define-test test-or
  (assert-equal
   (schemeval { (or false false) } *the-global-environment*)
   *scheme-false-value*)
  (assert-equal
   (schemeval { (or true) } *the-global-environment*)
   *scheme-true-value*)
  (assert-equal
   (schemeval { (or false false true false) } *the-global-environment*)
   *scheme-true-value*))
	      
