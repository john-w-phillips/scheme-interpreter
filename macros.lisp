(in-package :scheme-compiler)
(defclass macro (analyzed-syntax)
  ((variables :initarg :vars :reader macro-vars)
   (environment :initarg :env :reader macro-environment)
   (name :initarg :name :reader macro-name)
   (body :initarg :body :reader macro-body)))
(defmethod initialize-instance :after ((a-macro macro) &key)
  (with-slots (body) a-macro
    (setf (slot-value a-macro 'thunk)
	  (sequential-lambda
	   (mapcar #'schemeanalyze body)))))

(defun macro? (value)
  (eq (type-of value) 'macro))

(defun macro-define? (expr)
  (tagged-list? expr 'define-macro))

(defun make-macro-from-definition (expr env)
  (make-instance 'macro
		 :vars (macro-definition-vars expr)
		 :body (macro-definition-body expr)
		 :name (macro-definition-name expr)
		 :env env))
(defun macro-definition-name (expr)
  (caadr expr))

(defun macro-definition-vars (macro-expr)
  (cdadr macro-expr))

(defun macro-definition-body (macro-expr)
  (cddr macro-expr))


(defun eval-macro-define (macro-definition env)
  (let ((macro-name (macro-definition-name macro-definition))) 
    (assign-value macro-name
		  (make-macro-from-definition macro-definition env)
		  env))
  *define-return*)
(defun analyze-macro-definition (macro-definition)
  (let ((macro-name (macro-definition-name macro-definition))
	(macro-body (macro-definition-body macro-definition))
	(macro-vars (macro-definition-vars macro-definition)))
    (make-analyzed-syntax
     (lambda (env)
       (assign-value
	macro-name
	(make-instance 'macro
		       :vars macro-vars
		       :body macro-body
		       :name macro-name
		       :env env)
	env)))))

(put-syntax #'macro-define? #'eval-macro-define #'analyze-macro-definition)
