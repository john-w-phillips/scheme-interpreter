(in-package :scheme-compiler)
(defun macro? (value)
  (tagged-list? value 'macro-definition))

(defun macro-define? (expr)
  (tagged-list? expr 'define-macro))

(defun make-macro-from-definition (expr env)
  (list 'macro-definition
	(list 
	 (macro-definition-vars expr)
	 (macro-definition-body expr))
	env))
(defun macro-definition-name (expr)
  (caadr expr))

(defun macro-definition-vars (macro-expr)
  (cdadr macro-expr))

(defun macro-definition-body (macro-expr)
  (cddr macro-expr))

(defun macro-environment (macro)
  (caddr macro))

(defun macro-body (macro)
  (cadadr macro))

(defun macro-vars (macro)
  (caadr macro))


(defun eval-macro-define (macro-definition env)
  (let ((macro-name (macro-definition-name macro-definition))) 
    (assign-value macro-name
		  (make-macro-from-definition macro-definition env)
		  env))
  *define-return*)

(put-syntax #'macro-define? #'eval-macro-define)
