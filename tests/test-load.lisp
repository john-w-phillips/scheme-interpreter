(in-package :scheme-compiler-tests)

(define-test test-load
  (is
   equal
   6
   (uiop:call-with-temporary-file
    (lambda (stream pathname)
      (write-string "6" stream)
      (finish-output stream)
      (schemeval
       (with-input-from-string
	   (is (concatenate 'string
			    "(load \"" (namestring pathname) "\")"))
	 (let ((*package* (find-package :scheme-compiler)))
	   (read is)))
       *the-global-environment*))
    :want-stream-p t
    :want-pathname-p t)))

(define-test test-load-multiform
  (is
   equal
   64
   (uiop:call-with-temporary-file
    (lambda (stream pathname)
      (write-string "(define (f x) (* x x))" stream)
      (write-string "(define (g x) (* x x x))" stream)
      (write-string "(g (f 2))" stream)
      (finish-output stream)
      (schemeval
       (with-input-from-string
	   (is (concatenate 'string
			    "(load \"" (namestring pathname) "\")"))
	 (let ((*package* (find-package :scheme-compiler)))
	   (read is)))
	 *the-global-environment*))
      :want-stream-p t
      :want-pathname-p t)))

