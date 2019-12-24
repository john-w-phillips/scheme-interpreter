(in-package :scheme-compiler-tests)

(define-test test-macro-accessors
  (is
   equal
   (macro-definition-name { (define-macro (when x body) (list 'if x body 'false)) })
   { when })
  (true
   (macro-define? { (define-macro (when x body) (list 'if x body 'false)) }))
  (true
   (macro? (make-macro-from-definition
	    { (define-macro (when x body) (list 'if x body 'false)) } 'ENV)))
  (is
   equalp
   (macro-definition-body
    '(define-macro (when x body) (list 'if x body 'false)))
    (list '(list 'if x body 'false)))
  (is
   equalp
   (macro-definition-vars
    '(define-macro (when x body) (list 'if x body 'false)))
    '(x body)))

(define-test test-simple-macro
  (is
   equal 15
		(progn
		  (schemeval { (define-macro (when x body)
				   (list 'if x body 'false)) }
				   *the-global-environment*)
		  (schemeval { (when true 15) }
			     *the-global-environment*))))
