(in-package :scheme-compiler)

(defvar *syntax-table* '())
(defstruct syntax-item
  predicate
  evaluator
  analyzer)

(defun dispatch-syntax (syntaxitem exprs env)
  (funcall (syntax-item-evaluator syntaxitem) exprs env))

(defun analyze-syntax (syntaxitem exprs)
  (funcall (syntax-item-analyzer syntaxitem) exprs))

(defun find-syntax (exprs)
  (let ((found (remove-if
		(lambda (item)
		  (null
		   (funcall (syntax-item-predicate item)
			    exprs)))
		*syntax-table*)))
    (if found
	(car found)
	nil)))
(defclass analyzed-syntax ()
    ((thunk :initarg :thunk)))

(defun make-analyzed-syntax (thunk)
  (make-instance 'analyzed-syntax :thunk thunk))
(defmethod execute-syntax ((syn analyzed-syntax) env)
  (funcall (slot-value syn 'thunk) env))
(defun put-syntax (predicate evaluation &optional (analyzer nil))
  (setq *syntax-table*
	(cons
	 (make-syntax-item
	  :predicate predicate
	  :evaluator evaluation
	  :analyzer analyzer)
	 *syntax-table*)))

