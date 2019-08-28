(in-package :scheme-compiler)

(defvar *syntax-table* '())
(defstruct syntax-item
  predicate
  evaluator)

(defun dispatch-syntax (syntaxitem exprs env)
  (funcall (syntax-item-evaluator syntaxitem) exprs env))
   
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

(defun put-syntax (predicate evaluation)
  (setq *syntax-table*
	(cons
	 (make-syntax-item
	  :predicate predicate
	  :evaluator evaluation)
	 *syntax-table*)))

