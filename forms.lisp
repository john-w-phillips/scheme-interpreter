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

(put-syntax #'self-evaluating? #'eval-self-evaluating)

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

(defun quasiquote-text (expr env)
  (cond
    ((null expr) '())
    ((consp expr)
     (cons
      (quasiquote-text (car expr) env)
      (quasiquote-text (cdr expr) env)))
    (t
     (quasiquote-atom expr env))))

(defun quotation-text (expr env)
  (cond
    ((eql 'sb-int:quasiquote (car expr))
     (quasiquote-text (cadr expr) env))
    (t
     (cadr expr))))
   
(put-syntax #'quoted? #'quotation-text)


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

(put-syntax #'if? #'eval-if)


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
(put-syntax #'definition? #'eval-definition)


;; Variable forms.
(defun variable? (expr)
  (symbolp expr))

(defun lookup-variable (var env)
  (environment-lookup-val env var))
(put-syntax #'variable? #'lookup-variable)


;; Procedure forms.
(defun procedure? (obj)
  (tagged-list? obj 'lambda))

(defun procedure-body (proc)
  (cddar proc))

(defun procedure-vars (proc)
  (cadar proc))

(defun procedure-env (proc)
  (cadr proc))

(defun make-procedure (vars body env)
  (list (append (list 'lambda vars) body) env))

(defun make-procedure-from-lambda (lamb env)
  (list lamb env))

(defun eval-lambda (exp env)
  (make-procedure-from-lambda exp env))
(put-syntax #'procedure? #'eval-lambda)

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
(put-syntax #'and? #'eval-and)


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
(put-syntax #'or? #'eval-or)
  
