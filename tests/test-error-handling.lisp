(in-package :scheme-compiler-tests)

(define-test test-error-handling
  (assert-equal
   "error"
   (schemeval { (try-except (/ 1 0) "error") }
	      *the-global-environment*)))
