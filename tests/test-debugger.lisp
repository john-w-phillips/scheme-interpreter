(in-package :scheme-compiler-tests)
(require 'cl-ppcre)

(defclass test-view (viewer)
  ((expected-options-list :initarg :expected-options-list)
   (expected-outputs-list :initarg :expected-outputs-list)
   (keys-to-ask-for :initarg :keys-to-ask-for)))
(defmethod view-options ((view test-view) title &rest options)
  (with-slots (expected-options-list) view
    (assert-equalp
     expected-options-list
     (mapcar #'scheme-compiler:option-key options))))

(defmethod view-get-key-input ((view test-view))
  (with-slots (keys-to-ask-for) view
    (let ((first-key (car keys-to-ask-for)))
      (setq keys-to-ask-for (cdr keys-to-ask-for))
      first-key)))

(defmethod view-send-results ((view test-view) &rest outputs)
  (with-slots (expected-outputs-list) view
    (mapcar
     (lambda (a b)
       (assert-true (not (null (ppcre:scan a (string-value b))))))
     (car expected-outputs-list)
     outputs)
    (setq expected-outputs-list (cdr expected-outputs-list))))


(define-test test-debugger-display
    (let ((env
	   (extend-environment
	    the-empty-environment
	    (make-frame
	     (list '+ 'a)
	     (list (make-primitive-procedure '+) 2)))))
    (progn
      (let ((view
	     (make-instance 'test-view
			    :expected-options-list
			    (list scheme-compiler:PRINT-FRAME
				  scheme-compiler:UP
				  scheme-compiler:QUIT)
			    :keys-to-ask-for (list scheme-compiler:PRINT-FRAME
						   scheme-compiler:UP
						   scheme-compiler:PRINT-FRAME
						   scheme-compiler:QUIT)
			    :expected-outputs-list
			    (list (list "\\+.*\\[primitive-procedure.*\\]")
				  (list "[aA].*2"))))
	    (control
	     (make-instance 'scheme-compiler:controller))
	    (debugger
	     (make-instance 'scheme-compiler:debugger
			    :error (make-instance
				    'scheme-compiler:scheme-primitive-error
				    :environment env
				    :ops '()
				    :op 'operation))))
       (interact-with-user control view debugger)))))
