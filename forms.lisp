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
       (tagged-list? expr 'quote)))
(defun quotation-text (expr env)
  (declare (ignore env))
  (cadr expr))
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
    (caddr expr))))

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
