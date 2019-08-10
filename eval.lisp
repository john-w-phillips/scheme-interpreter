(in-package :scheme-compiler)
(export '(*the-global-environment*
	  *define-return*))
(defvar *define-return* 'ok)

(defun make-cell (var val)
  (cons var val))

(defun cell-define-variable (cell)
  (car cell))
(defun cell-define-value (cell)
  (cdr cell))

(defun cell-assign-value (cell val)
  (setf (cdr cell) val))

(defun make-procedure (vars body env)
  (list 'lambda vars body env))

(defun make-primitive-procedure (proc)
  (list 'primitive-procedure proc))
(defun primitive-procedure-proc (proc)
  (cadr proc))

(defun primitive-procedure? (proc)
  (tagged-list? proc 'primitive-procedure))

(defun procedure? (obj)
  (tagged-list? obj 'lambda))

(defun procedure-body (proc)
  (caddr proc))

(defun procedure-vars (proc)
  (cadr proc))

(defun procedure-env (proc)
  (cadddr proc))

(defun tagged-list? (obj tag)
  (and
   (consp obj)
   (eql (car obj) tag)))


(defun make-frame (vars vals)
  (list 'frame
	(mapcar
	 (lambda (var val)
	   (make-cell var val))
	 vars
	 vals)))
(defun frame-define-cells (frame)
  (cadr frame))
(defun frame-empty? (obj)
  (null (cadr obj)))

(defun frame? (obj)
  (tagged-list? obj 'frame))


(defun frame-lookup-val (frame name)
  (let ((found (remove-if
		(lambda (obj)
		  (not (or
			(eq (car obj) name)
			(string= (car obj) name))))
		(frame-define-cells frame))))
    (if (not (null found))
	(car found)
      nil)))

(defun frame-assign-val (frame var val)
  (let ((found (frame-lookup-val frame var)))
    (if found
	(cell-assign-value found val)
      (if (frame-empty? frame)
	  (setf (cadr frame)
		(list (cons var val)))
	(setf (cdr (last (frame-define-cells
			  frame)))
	      (cons (cons var val) '()))))))

(defvar the-empty-environment '())
(defun extend-environment (env frame)
  (cons frame env))

(defun enclosing-environment (env)
  (cdr env))

(defun environment-first-frame (env)
  (car env))

(defun empty-environment? (environment)
  (null environment))

(defun environment-find-cell (environment var)
  (if (empty-environment? environment)
      nil
    (let ((found (frame-lookup-val
		  (environment-first-frame environment)
		  var)))
      (if (null found)
	  (environment-find-cell
	   (enclosing-environment environment)
	   var)
	found))))
	   
(defun environment-lookup-val (environ var)
  (let ((found (environment-find-cell environ var)))
    (if (null found)
	(error "Undefined variable -- ENVIRONMENT-LOOKUP-VAL")
      (cell-define-value found))))

(defun lookup-variable (var env)
  (environment-lookup-val env var))

(defun assign-value (var val env)
  (let ((cell (environment-find-cell env var)))
    (if cell
	(cell-assign-value cell val)
      (frame-assign-val (environment-first-frame env) var val))))

(defun application? (expr)
  (consp expr))

(defun self-evaluating? (expr)
  (or (numberp expr)
      (stringp expr)))

(defun quoted? (expr)
  (and (consp expr)
       (tagged-list? expr 'quote)))

(defun quotation-text (expr)
  (cadr expr))

(defun first-expr (exprs)
  (car exprs))
(defun rest-expr (exprs)
  (cdr exprs))

(defun schemeval-sequence (exprs env)
  (defun iter-sequence (last-value exprs)
    (if (null exprs)
	last-value
      (iter-sequence
       (schemeval (first-expr exprs) env)
       (rest-expr exprs))))
  (iter-sequence 'empty exprs))

(defun definition? (expr)
  (tagged-list?
   expr
   'define))

(defun if? (expr)
  (tagged-list? expr 'if))

(defun if-predicate (expr)
  (cadr expr))

(defun if-consequent (expr)
  (caddr expr))

(defun if-alternative (expr)
  (cadddr expr))

(defun scheme-true? (val)
  (or
   (eq val t)
   (eq val 'true)))

(defun eval-if (expr env)
  (let ((predicate (if-predicate expr))
	(consequent (if-consequent expr))
	(alternative (if-alternative expr)))
    (if (scheme-true? (schemeval predicate env))
	(schemeval consequent env)
      (schemeval alternative env))))

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

(defun variable? (expr)
  (symbolp expr))

(defun operator (expr)
  (car expr))

(defun operands (expr)
  (cdr expr))

(defun schemeval (expr env)
  (cond
   ((self-evaluating? expr)
    expr)
   ((variable? expr)
    (lookup-variable expr env))
   ((quoted? expr)
    (quotation-text expr))
   ((if? expr)
    (eval-if expr env))
   ((definition? expr)
    (eval-definition expr env))
   ((macro-definition? expr)
    (define-macro expr *macros-frame*))
   ((application? expr)
    (schemeapply (schemeval (operator expr) env)
		 (operands expr)
		 env))
   (t
    (error "Cannot evaluate "))))

(defun listof-arguments (exprs env)
  (if (null exprs)
      '()
    (cons (schemeval (first-expr exprs) env)
	  (listof-arguments
	   (rest-expr exprs)
	   env))))

(defun schemeapply (op ops env)
  (cond
   ((primitive-procedure? op)
    (apply (primitive-procedure-proc op) (listof-arguments ops env)))
   (t
    (schemeval-sequence
     (procedure-body op)
     (extend-environment
      (procedure-env op)
      (make-frame
       (procedure-vars op)
       (listof-arguments ops env)))))))

(defvar *global-primitives*
  (list (list '+ '+)
	(list '* '*)
	(list 'car 'car)
	(list 'cons 'cons)
	(list 'cdr 'cdr)
	(list '- '-)
	(list '< '<)
	(list '> '>)
	(list '= '=)))

(defun macro-definition? (expr)
  (tagged-list? expr 'define-macro))

(defun make-macro (macro-vars macro-body)
  (list 'macro macro-vars macro-body))

;; (defun macro-vars (macro)
(defun define-macro (expr macros-frame)
  (frame-assign-val
   macros-frame
   (macro-name expr)
   (make-macro 
    (macro-vars expr)
    (macro-body expr))))

  
(defun macro-definition-vars (expr)
  (cdadr expr))

(defun macro-definition-body (expr)
  (cddr expr))

(defun macro-definition-name (expr)
  (caadr expr))

(defun macro-application? (expr macros-frame)
  (and (consp expr)
       (not (null (frame-lookup-val
		   macros-frame
		   (car expr))))))

;; (defun expand-macro (expr macros-frame env)
;;   (let ((body
;; 	 (frame-lookup-val
;; 	  (car expr)
;; 	  macros-frame)))
;;     (schemeval-sequence
;;      body
;;      (extend-environment
;;       ;; (macro-vars ...)
;;       ;; (macro-exprs ...)
;;      env))))

(defun setup-environment (input-environ)
  (extend-environment
   input-environ
   (make-frame 
   (mapcar
    (lambda (x)
      (car x))
    *global-primitives*)
   (mapcar
    (lambda (x)
      (make-primitive-procedure (cadr x)))
    *global-primitives*))))

(defvar *the-global-environment*
  (setup-environment the-empty-environment))
(defvar *macros-frame*
  (make-frame '() '()))

  
(assign-value 'true t *the-global-environment*)
(assign-value 'false nil *the-global-environment*)
(defun user-print (input)
  (cond
   ((procedure? input)
    (print 'procedure))
   (t
    (print input))))
    
(defun driver-loop ()
  (let ((input (read)))
    (if (and (symbolp input) (string= input :repl-quit))
	'ok
	(progn
	  (user-print (schemeval input *the-global-environment*))
	  (finish-output *standard-output*)
	  (driver-loop)))))

