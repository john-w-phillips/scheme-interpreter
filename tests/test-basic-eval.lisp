(in-package :scheme-compiler-tests)
(define-test test-syntax-is-loaded
  (assert-true
   (not (null
	 (find-syntax { (if (x) 1 0) })))))

(define-test test-basic-eval
  (assert-equal (schemeval
		 4
		 *the-global-environment*) 4)
  (assert-equal (schemeval
		 { (define (f x) (* x 2)) }
		 *the-global-environment*)
		*define-return*))

(define-test test-definitions
  (assert-equal (schemeval
		  { (define (square x) (* x x)) }
		 *the-global-environment*)
		*define-return*)
  (assert-equal (schemeval  { (* 2 2) }
			   *the-global-environment*)
		4)
  (assert-equal (schemeval { (square 3) }
			   *the-global-environment*)
		9))

(define-test test-long-body
  (assert-equal (schemeval
		  { (define (longbody x y z) x y z) }
		 *the-global-environment*)
		*define-return*)
  (assert-equal (schemeval  { (longbody 1 2 3) }
			   *the-global-environment*)
		3))

(define-test test-syntax-lookup
  (let ((scheme-compiler:*syntax-table* '()))
    (let ((predicate
	   (lambda (x) (and (consp x)
			    (eql (car x) 'if))))
	  (evaller (lambda (x expr env)
		     (car expr))))
      (put-syntax
       predicate
       evaller)
      (assert-equalp
       (find-syntax '(if x 10))
       (make-syntax-item
	:predicate predicate
	:evaluator evaller)))))

(define-test test-lambda-accessors
  (assert-true
   (procedure? { (lambda (x) (+ x x)) }))
  (assert-equalp
   (procedure-body (schemeval { (lambda (x) (+ x x)) } *the-global-environment*))
   { ((+ x x)) })
  (assert-equalp
   (procedure-vars (schemeval { (lambda (x y) (+ x y)) } *the-global-environment*))
   { (x y) }))
	   
(define-test test-lambda-eval
  (assert-equal
   (schemeval
    { ((lambda (x) (+ x x)) 2) } *the-global-environment*)
    4)
  (assert-equal
   (schemeval
    { (((lambda (x)
	 (lambda (y)
	   (+ x y))) 2) 3) } *the-global-environment*)
   5))
	 
	    
(define-test test-if-then
  (assert-equal
   (schemeval
    { (if false 1 12) } *the-global-environment*)
   12))
