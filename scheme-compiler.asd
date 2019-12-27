(defsystem :scheme-compiler
  :name "scheme-compiler"
  :version "0.0.0"
  :depends-on (#:cl-ppcre #:equals)
  ;; :build-operation "program-op"
  ;; :build-pathname "scheme"
  ;; :entry-point "scheme-compiler:driver-loop"
  :in-order-to ((test-op (test-op "scheme-compiler/tests")))
  :components ((:file "package")
	       (:file "common")
	       (:file "syntax")
	       (:file "primitives")
	       (:file "procedure")
	       (:file "environment")
	       (:file "forms")
	       (:file "macros")
	       (:file "eval")
	       (:file "load")
	       (:file "debugger")))
(defsystem "scheme-compiler/executable"
  :build-operation "program-op"
  :build-pathname "scheme"
  :entry-point "scheme-compiler:driver-loop"
  :depends-on ("scheme-compiler"))

(defsystem "scheme-compiler/tests"
  :depends-on ("scheme-compiler" #:parachute)
  :pathname "tests/"
  :components ((:file "package")
	       (:file "tests")
	       (:file "test-basic-eval")
	       (:file "test-quotations")
	       (:file "test-environment")
	       (:file "test-and-or")
	       (:file "test-apply")
	       (:file "test-load")
	       (:file "test-assignment")
	       (:file "test-procs")
	       (:file "test-macros")
	       (:file "test-debugger")
	       (:file "test-error-handling"))
  :perform (test-op (o c)
		    (uiop:symbol-call :scheme-compiler-tests
				      :run-all-tests)))

