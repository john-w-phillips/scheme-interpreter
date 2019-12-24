(in-package :scheme-compiler-tests)

(define-test test-quotation-text-predicate
  (true (quoted?
		(with-input-from-string
		    (is "'(cat)")
		  (read is))))
  (true (quoted?
		(with-input-from-string
		    (is "`(cat)")
		  (read is)))))

(define-test test-quotation-text-simple
  (is
   equalp
   (quotation-text
    (with-input-from-string
	(is "'(a quoted (list of) lists)")
      (read is)) *the-global-environment*)
   '(a quoted (list of) lists)))

(define-test test-quasiquote-text
  (is
   equalp
   { (a quoted (list of) lists) }
   (progn
     (schemeval { (define outside-list (list 'list 'of)) }
		*the-global-environment*)
     (quotation-text
      (let ((*package* (find-package :scheme-compiler)))
	(with-input-from-string
	    (is "`(a quoted ,outside-list lists)")
	  (read is)))
      *the-global-environment*))))
    
