(defsystem :scheme-compiler
  :name "scheme-compiler"
  :version "0.0.0"
  :depends-on (#:cl-ppcre #:parachute #:equals)
  :build-operation "program-op"
  :build-pathname "scheme"
  :entry-point "scheme-compiler:driver-loop"
  :components ((:file "package")
	       (:file "common")
	       (:file "syntax")
	       (:file "primitives")
	       (:file "environment")
	       (:file "forms")
	       (:file "macros")
	       (:file "eval")
	       (:file "load")
	       (:file "debugger")
	       (:module tests
			:depends-on ("eval")
			:components ((:file "tests")
				     (:file "test-basic-eval")
				     (:file "test-quotations")
				     (:file "test-environment")
				     (:file "test-and-or")
				     (:file "test-apply")
				     (:file "test-load")
				     (:file "test-assignment")
				     (:file "test-macros")
				     (:file "test-debugger")
				     (:file "test-error-handling")))))

  
