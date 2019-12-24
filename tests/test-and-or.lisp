(in-package :scheme-compiler-tests)

(define-test test-and
  (true
   (schemeval { (and true true true) } *the-global-environment*))
  (true
   (schemeval { (and) } *the-global-environment*))
  (is
   equal
   *scheme-false-value*
   (schemeval { (and true true true false) } *the-global-environment*)))

(define-test test-or
  (is
   equal
   (schemeval { (or false false) } *the-global-environment*)
   *scheme-false-value*)
  (is
   equal
   (schemeval { (or true) } *the-global-environment*)
   *scheme-true-value*)
  (is
   equal
   (schemeval { (or false false true false) } *the-global-environment*)
   *scheme-true-value*))
	      
