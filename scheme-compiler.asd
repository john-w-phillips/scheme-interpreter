(defsystem :scheme-compiler
  :name "scheme-compiler"
  :version "0.0.0"
  :depends-on (#:lisp-unit)
  :build-operation "program-op"
  :build-pathname "scheme"
  :entry-point "scheme-compiler:driver-loop"
  :components ((:file "package")
	       (:file "syntax")
	       (:file "primitives")
	       (:file "environment")
	       (:file "forms")
	       (:file "macros")
	       (:file "eval")
	       (:module tests
			:depends-on ("eval")
			:components ((:file "test-basic-eval")
				     (:file "test-quotations")
				     (:file "test-environment")
				     (:file "test-macros")))))

  
