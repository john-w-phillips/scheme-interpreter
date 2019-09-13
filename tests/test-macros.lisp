(in-package :scheme-compiler-tests)

(define-test test-macro-accessors
  (assert-equal
   (macro-definition-name { (define-macro (when x body) (list 'if x body 'false)) })
   { when })
  (assert-true
   (macro-define? { (define-macro (when x body) (list 'if x body 'false)) }))
  (assert-true
   (macro? (make-macro-from-definition
	    { (define-macro (when x body) (list 'if x body 'false)) } 'ENV)))
  (assert-equalp
   (macro-definition-body
    '(define-macro (when x body) (list 'if x body 'false)))
    (list '(list 'if x body 'false)))
  (assert-equalp
   (macro-definition-vars
    '(define-macro (when x body) (list 'if x body 'false)))
    '(x body)))

(define-test test-simple-macro
  (assert-equal 15
		(progn
		  (schemeval { (define-macro (when x body)
				   (list 'if x body 'false)) }
				   *the-global-environment*)
		  (schemeval { (when true 15) }
			     *the-global-environment*))))
