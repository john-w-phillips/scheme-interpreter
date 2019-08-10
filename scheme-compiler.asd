(defpackage :scheme-compiler
  (:use :cl :asdf)
  (:export driver-loop
	   schemeval
	   schemeapply))
(defpackage :scheme-compiler-tests
  (:use :cl :asdf :lisp-unit :scheme-compiler))

(in-package :scheme-compiler)

(defsystem :scheme-compiler
  :name "scheme"
  :version "0.0.0"
  :components ((:file "eval")
	       (:module tests
			:depends-on ("eval")
			:components ((:file "test-basic-eval")))))

  
