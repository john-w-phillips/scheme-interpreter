(in-package :scheme-compiler)
(defun tagged-list? (obj tag)
  (and
   (consp obj)
   (eql (car obj) tag)))

;; Self-evaluating forms. 
(defun self-evaluating? (expr)
  (or (numberp expr)
      (stringp expr)))

(defun eval-self-evaluating (expr env)
  (declare (ignore env))
  expr)
(defun analyze-self-evaluating (expr)
  (make-analyzed-syntax
   (lambda (env)
     (declare (ignore env))
     expr)))

(put-syntax #'self-evaluating? #'eval-self-evaluating
	    #'analyze-self-evaluating)

;; Quotation forms
(defun quoted? (expr)
  (and (consp expr)
       (or
	(tagged-list? expr 'quote)
	(tagged-list? expr 'sb-int:quasiquote))))

;; (defun quotation-text (expr env)
;;   (declare (ignore env))
;;   (cadr expr))
(defun quasiquote-atom (expr env)
  (cond
    ((sb-impl::comma-p expr)
     (schemeval (sb-impl::comma-expr expr) env))
    (t expr)))

(defconstant at-kind 2 "Constant type of @ splices, ripped from SBCL code")
(defun comma-at-splice? (expr)
  (and
   (sb-impl::comma-p expr)
   (= (sb-impl::comma-kind expr) at-kind)))

(defun quasiquote-text (expr env)
  (cond
    ((null expr) '())
    ((consp expr)
     (let ((cdr-text (quasiquote-text (cdr expr) env))
	   (car-text (quasiquote-text (car expr) env)))
       (if (comma-at-splice? (car expr))
	   (append
	    car-text
	    cdr-text)
	   (cons car-text cdr-text))))
    (t
     (quasiquote-atom expr env))))

(defun quotation-text (expr env)
  (cond
    ((eql 'sb-int:quasiquote (car expr))
     (quasiquote-text (cadr expr) env))
    (t
     (cadr expr))))

(defun analyze-quasiquote-atom (expr)
  (cond
    ((sb-impl::comma-p expr)
     (schemeanalyze expr))
    (t
     (make-analyzed-syntax
      (lambda (env)
	(declare (ignore env))
	expr)))))

(defun analyze-quasiquote (expr)
  (cond
    ((null expr)
     (make-analyzed-syntax
      (lambda (env)
	(declare (ignore env))
	nil)))
    ((consp expr)
     (let ((analyzed-cdr (analyze-quasiquote (cdr expr)))
	   (analyzed-car (analyze-quasiquote (car expr))))
       (make-analyzed-syntax
	(lambda (env)
	  (append
	   (execute-syntax analyzed-car env)
	   (execute-syntax analyzed-cdr env))))
       (make-analyzed-syntax
	(lambda (env)
	  (cons (execute-syntax analyzed-car env)
		(execute-syntax analyzed-cdr env))))))
    (t
     (analyze-quasiquote-atom expr))))
	  					     
(defun analyze-quotation (expr)
  (cond
    ((eql 'sb-int:quasiquote (car expr))
     (analyze-quasiquote expr))
    (t
     (make-analyzed-syntax (lambda (env)
			     (declare (ignore env))
			     (cadr expr))))))

(put-syntax #'quoted? #'quotation-text #'analyze-quotation)


(defun scheme-true? (val)
  (or
   (eq val t)
   (eq val 'true)))


;; If forms
(defun if? (expr)
  (tagged-list? expr 'if))

(defun if-predicate (expr)
  (cadr expr))

(defun if-consequent (expr)
  (caddr expr))

(defun if-alternative (expr)
  (cadddr expr))

(defun eval-if (expr env)
  (let ((predicate (if-predicate expr))
	(consequent (if-consequent expr))
	(alternative (if-alternative expr)))
    (if (scheme-true? (schemeval predicate env))
	(schemeval consequent env)
	(schemeval alternative env))))
(defun analyze-if (expr)
  (let ((predicate-code (schemeanalyze (if-predicate expr)))
	(consequent-code (schemeanalyze (if-consequent expr)))
	(alternative-code (schemeanalyze (if-alternative expr))))
    (make-analyzed-syntax
     (lambda (env)
       (if (scheme-true? (execute-syntax predicate-code env))
	   (execute-syntax consequent-code env)
	   (execute-syntax alternative-code env))))))
(put-syntax #'if? #'eval-if #'analyze-if)


;; Definition forms
(defun definition? (expr)
  (tagged-list?
   expr
   'define))


(defun definition-var (expr)
  (cond
   ((consp (cadr expr))
    (caadr expr))
   (t (cadr expr))))
(defun analyze-definition-value (expr)
  (cond
    ((consp (cadr expr))
     (make-analyzed-proc (cdadr expr)
			 (cddr expr)))
    (t
     (schemeanalyze (caddr expr)))))

(defun definition-value (expr env)
  (cond
   ((consp (cadr expr))
    (make-procedure
     (cdadr expr)
     (cddr expr)
     env))
   (t
    (schemeval (caddr expr) env))))

(defun eval-definition (expr env)
  (assign-value
    (definition-var expr)
    (definition-value expr env)
    env)
  *define-return*)
(defun analyze-definition (expr)
  (let ((var (definition-var expr))
	(value (analyze-definition-value expr)))
    (make-analyzed-syntax
     (lambda (env)
       (assign-value
	var
	(execute-syntax value env)
	env)
       *define-return*))))

(put-syntax #'definition? #'eval-definition #'analyze-definition)


;; Variable forms.
(defun variable? (expr)
  (symbolp expr))

(defun lookup-variable (var env)
  (environment-lookup-val env var))
(defun analyze-variable (expr)
  (make-analyzed-syntax
   (lambda (env) (environment-lookup-val env expr))))

(put-syntax #'variable? #'lookup-variable #'analyze-variable)


;; Procedure forms.
(defun make-procedure (vars body env)
  (make-instance 'procedure
		 :vars vars
		 :body body
		 :env env))
(defun make-analyzed-proc (vars body)
  (make-analyzed-syntax
   (lambda (env)
     (make-instance 'procedure
		    :vars vars
		    :body body
		    :env env))))


(defun procedure? (obj) (eq (type-of obj) 'procedure))

(defun lambda-vars (lambda-form)
  (cadr lambda-form))
(defun lambda-body (lambda-form)
  (cddr lambda-form))
(defun lambda? (exp)
  (tagged-list? exp 'lambda))

	  
(defun make-procedure-from-lambda (lamb env)
  (make-instance
   'procedure
   :vars (lambda-vars lamb)
   :env env
   :body (lambda-body lamb)))

(defun analyze-lambda (exp)
  (let ((vars (lambda-vars exp))
	(body (lambda-body exp)))
    (make-analyzed-syntax
     (lambda (env)
       (make-instance
	'procedure
	:vars vars
	:env env
	:body body)))))
(defun eval-lambda (exp env)
  (make-procedure-from-lambda exp env))
(put-syntax #'lambda? #'eval-lambda #'analyze-lambda)
(export '(make-procedure
	  lambda-vars
	  lambda-body
	  lambda?
	  make-procedure-from-lambda
	  eval-lambda))
;; and
(defun and? (expr)
  (tagged-list? expr 'and))
(defun and-ops (expr)
  (cdr expr))
(defun first-op (ops)
  (car ops))
(defun rest-ops (ops)
  (cdr ops))

(defun eval-and-ops (ops env)
  (cond
    ((null ops) *scheme-true-value*)
    (t
     (if (scheme-true? (schemeval (first-op ops) env))
	 (eval-and-ops (rest-ops ops) env)
	 *scheme-false-value*))))
(defun eval-and (expr env)
  (eval-and-ops (and-ops expr) env))
(defun analyze-and (expr)
  (let* ((ops (and-ops expr))
	 (analyzedops (mapcar #'schemeanalyze ops)))
    (make-analyzed-syntax
     (lambda (env)
       (let ((rval *scheme-true-value*))
	 (dolist (an-op analyzedops rval)
	   (if (not (scheme-true? (execute-syntax an-op env)))
	       (progn
		 (setq rval *scheme-false-value*)
		 (return rval)))))))))
(put-syntax #'and? #'eval-and #'analyze-and)


;; or
(defun or? (expr)
  (tagged-list? expr 'or))
(defun or-ops (expr)
  (cdr expr))
(defun eval-or-ops (ops env)
  (cond
    ((null ops) *scheme-false-value*)
    (t
     (if (scheme-true? (schemeval (first-op ops) env))
	 *scheme-true-value*
	 (eval-or-ops (rest-ops ops) env)))))
(defun eval-or (expr env)
  (eval-or-ops (or-ops expr) env))

(defun analyze-or (expr)
  (let* ((ops (or-ops expr))
	 (analyzed-ops (mapcar #'schemeanalyze ops)))
      (make-analyzed-syntax
       (lambda (env)
	 (let ((rval *scheme-false-value*))
	   (dolist (an-op analyzed-ops rval)
	     (when (scheme-true? (execute-syntax an-op env))
	       (progn
		 (setq rval *scheme-true-value*)
		 (return rval)))))))))

(put-syntax #'or? #'eval-or #'analyze-or)
  
(defun assignment? (expr)
  (tagged-list? expr 'set!))

(defun assignment-variable (expr)
  (cadr expr))

(defun assignment-value-expr (expr)
  (caddr expr))

(defun eval-assignment (expr env)
  (let ((var (assignment-variable expr))
	(value (schemeval (assignment-value-expr expr)
			  env)))
    (assign-value var value env)))
(defun analyze-assignment (expr)
  (let ((var (assignment-variable expr))
	(value (schemeanalyze (assignment-value-expr expr))))
    (make-analyzed-syntax
     (lambda (env)
       (assign-value var (execute-syntax value env) env)))))

(put-syntax #'assignment? #'eval-assignment #'analyze-assignment)

(defun try-except? (expr)
  (tagged-list? expr 'try-except))

(defun try-except-tryexpr (expr)
  (cadr expr))

(defun try-except-exceptexpr (expr)
  (caddr expr))

(defun eval-try-except (expr env)
  (let ((tryexpr (try-except-tryexpr expr))
	(except-expr (try-except-exceptexpr expr)))
    (handler-case
	(schemeval tryexpr env)
      (scheme-error (err)
	(schemeval except-expr env)))))
(defun analyze-try-except (expr)
  (let ((tryexpr (schemeanalyze (try-except-tryexpr expr)))
	(except-expr (schemeanalyze (try-except-exceptexpr expr))))
    (make-analyzed-syntax
     (lambda (env)
       (handler-case
	   (execute-syntax tryexpr env)
	 (scheme-error (err)
	   (execute-syntax except-expr env)))))))
(put-syntax #'try-except? #'eval-try-except #'analyze-try-except)
