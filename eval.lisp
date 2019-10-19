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
  "Given exprs and env, eval all exprs in the proper order, 
and return evaled args"
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

(defun debugger-get-option-number ()
  (parse-integer (read-line)))

(defun print-debug-options ()
  (format t ";; An Error has been detected. Please pick an option: ~%")
  (format t ";; => 1. Continue and return nil ~%")
  (format t ";; => 2. Quit ~%"))

(defun debugger (the-error)
  (print-debug-options)
  (let ((option-number (debugger-get-option-number)))
    (cond
      ((= option-number 1)
       nil)
      ((= option-number 2)
       (scheme-quit "User requested quit"))
      (t
       (format t ";;; Invalid debugger input: ~a quitting...~%"
	       option-number)
       (scheme-quit "Bad option")))))

(defun handle-error (input)
  (handler-case
      (schemeval input *the-global-environment*)
    (error (c)
      (debugger c))))
	      
(defun evaluate-input (input)
  (if (and (symbolp input) (string= input :repl-quit))
      'ok
      (progn
	(let ((output (handle-error input)))
	  (declare-output)
	  (user-print output)
	  (finish-output *standard-output*)))))

(define-condition scheme-quit (error)
  ((message :initarg :message)))

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
