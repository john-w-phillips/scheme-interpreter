(in-package :scheme-compiler)
(defvar *define-return* 'ok)

(defun read-scheme-code (stream char)
  (declare (ignore char))
  (let ((*package* (find-package "SCHEME-COMPILER")))
    (let ((read-list (list (quote quote) (read stream t nil t))))
      (do ((c (read-char stream) (read-char stream)))
	  ((eql c #\}) read-list)))))

(set-macro-character #\{ #'read-scheme-code)

(defun application? (expr)
  (consp expr))

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



(defun operator (expr)
  (car expr))

(defun operands (expr)
  (cdr expr))

(defun interpreter-apply (op &rest args)
  "APPLY in the scheme, which takes no environment argument"
  (cond
    ((null args)
     (error (format nil "The apply procedure has been called with no arguments")))
    ((null (cdr args))
     (apply #'schemeapply (list op (car args) *the-global-environment*)))
    (t
     (apply #'schemeapply (list op args *the-global-environment*)))))
(defun schemeanalyze (expr)
  (let ((found-syntax (find-syntax expr)))
    (cond
      ((not (null found-syntax))
       (analyze-syntax found-syntax expr))
      ((application? expr)
       (schemeanalyze-application expr)))))
(defun listof-analyzed-exprs (exprs)
  (mapcar #'schemeanalyze exprs))

(defun schemeanalyze-application (expr)
    (let ((args (listof-analyzed-exprs (operands expr)))
	  (operation (schemeanalyze (operator expr))))
      (make-analyzed-syntax
       (lambda (env)
	 (let ((evaled-args (mapcar (lambda (arg) (execute-syntax arg env)) args))
	       (evaled-op (execute-syntax operation env)))
	   (cond
	     ((primitive-procedure? evaled-op)
	      (apply-primitive-op evaled-op evaled-args env))
	     ((macro? evaled-op)
	      (schemeval
	       (execute-syntax
		evaled-op
		(extend-environment
		 (macro-environment evaled-op)
		 (make-frame
		  (macro-vars evaled-op)
		  (operands expr))))
	       env))
	     ((procedure? evaled-op)
	      (execute-syntax
	       evaled-op
	       (extend-environment
		(procedure-env evaled-op)
		(make-frame
		 (procedure-vars evaled-op)
		 evaled-args))))
	     (t (error "Unknown syntax type"))))))))
(defun schemeval (expr env)
  (execute-syntax (schemeanalyze expr) env))

;; (defun schemeval (expr env)
;;   (let ((found-syntax (find-syntax expr)))
;;     (cond
;;       ((not (null found-syntax))
;;        (dispatch-syntax found-syntax expr env))
;;       ((application? expr)
;;        (schemeapply (schemeval (operator expr) env)
;; 		    (operands expr)
;; 		    env))
;;       (t (error (format nil  "Cannot evaluate ~a~%" expr))))))

(defun listof-arguments (exprs env)
  "Given exprs and env, eval all exprs in the proper order, 
and return evaled args"
  (if (null exprs)
      '()
    (cons (schemeval (first-expr exprs) env)
	  (listof-arguments
	   (rest-expr exprs)
	   env))))

(defun apply-primitive-op (op ops env)
  (handler-case
      (apply (primitive-procedure-proc op) ops)
    (error (e)
      (scheme-primitive-error :error e
			      :environment env
			      :op op
			      :ops ops))))

(defun schemeapply (op ops env)
  (cond
    ((primitive-procedure? op)
     (apply-primitive-op op ops env))
   ((macro? op)
    (schemeval (schemeval-sequence
		(macro-body op)
		(extend-environment
		 (macro-environment op)
		 (make-frame
		  (macro-vars op)
		  ops)))
	       env))
   (t
    (schemeval-sequence
     (procedure-body op)
     (extend-environment
      (procedure-env op)
      (make-frame
       (procedure-vars op)
       (listof-arguments ops env)))))))

(defun ask-for-input ()
  (format t ";;; Eval => ~%")
  (finish-output *standard-output*))
(defun declare-output ()
  (format t ";;; Value => "))

(defun user-print (input)
  (cond
   ((procedure? input)
    (print 'procedure))
   ((eql input *scheme-true-value*)
    (print "#t"))
   ((eql input *scheme-false-value*)
    (print "#f"))
   (t
    (print input)))
  (format t "~%"))

(defun handle-error (input)
  (handler-case
      (schemeval input *the-global-environment*)
    (error (c)
      (progn
	(format t "Caught error ~a" (scheme-parent-error c))
	(console-debugger-debug c)))))
	      
(defun evaluate-input (input)
  (if (and (symbolp input) (string= input :repl-quit))
      'ok
      (progn
	(let ((output (handle-error input)))
	  (declare-output)
	  (user-print output)
	  (finish-output *standard-output*)))))

(define-condition scheme-error (error) ())
(define-condition scheme-quit (scheme-error)
  ((message :initarg :message)))

(define-condition scheme-primitive-error (scheme-error)
  ((env :initarg :environment)
   (op :initarg :op)
   (ops :initarg :ops)
   (error :initarg :error :reader scheme-parent-error)))

(defun scheme-primitive-error (&key error environment op ops)
  (error 'scheme-primitive-error
	 :environment environment
	 :error error
	 :op op
	 :ops ops))
    
(defun scheme-quit (message)
  (error 'scheme-quit :message message))

(defun driver-loop ()
  (handler-bind
      ((scheme-quit #'(lambda (c) (format t ";;; Quitting scheme...~%")
			      (return-from driver-loop))))
    (let ((*package* (find-package :scheme-compiler)))
      (ask-for-input)
      (handler-case
	  (progn
	    (let ((the-input (read)))
	      (evaluate-input the-input)
	      (driver-loop)))
	(end-of-file (var) (declare (ignore var))
		     (format t ";;; EOF~%")
		     (scheme-quit "EOF"))))))
