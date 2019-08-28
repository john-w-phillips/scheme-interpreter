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

(defun schemeval (expr env)
  (let ((found-syntax (find-syntax expr)))
    (cond
      ((not (null found-syntax))
       (dispatch-syntax found-syntax expr env))
      ((application? expr)
       (schemeapply (schemeval (operator expr) env)
		    (operands expr)
		    env))
      (t (error (format nil  "Cannot evaluate ~a~%" expr))))))

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

