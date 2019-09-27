(in-package :scheme-compiler-tests)

(define-test test-assignment
  (assert-equal
   10
   (progn
     (mapcar
      (lambda (expr) (schemeval expr *the-global-environment*))
      (list { (define x 2) }
	    { (set! x 6) }
	    { (set! x (+ x 4)) }))
     (schemeval { x } *the-global-environment*))))
