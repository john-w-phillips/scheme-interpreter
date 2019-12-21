(in-package :scheme-compiler)
(defclass environment ()
  ((frame-list :initarg :frames)))


(defun make-cell (var val)
  (cons var val))

(defun cell-define-variable (cell)
  (car cell))
(defun cell-define-value (cell)
  (cdr cell))

(defun cell-assign-value (cell val)
  (setf (cdr cell) val))

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

(defvar the-empty-environment (make-instance 'environment :frames '()))
(defmethod extend-environment ((env environment) frame)
  (with-slots (frame-list) env
    (make-instance
     'environment
     :frames (cons frame frame-list))))

(defmethod  enclosing-environment ((env environment))
  (with-slots (frame-list) env
    (make-instance
     'environment
     :frames (cdr frame-list))))

(defmethod environment-first-frame ((env environment))
  (with-slots (frame-list) env
    (car frame-list)))

(defmethod empty-environment? ((env environment))
  (with-slots (frame-list) env
    (null frame-list)))

(defmethod environment-find-cell ((env environment) var)
  (if (empty-environment? env)
      nil
      (let ((found (frame-lookup-val
		    (environment-first-frame env)
		    var)))
	(if (null found)
	    (environment-find-cell
	     (enclosing-environment env)
	     var)
	    found))))

(defmethod environment-lookup-val ((environ environment) var)
  (let ((found (environment-find-cell environ var)))
    (if (null found)
	(error (format nil "Undefined variable -- ENVIRONMENT-LOOKUP-VAL ~a" var))
      (cell-define-value found))))



(defun assign-value (var val env)
  (let ((cell (environment-find-cell env var)))
    (if cell
	(cell-assign-value cell val)
	(frame-assign-val (environment-first-frame env) var val))))

(defun interpreter-get-internal-time-ms ()
  (get-internal-real-time))

(defvar *global-primitives*
  (list (list '+ '+)
	(list '* '*)
	(list '/ '/)
	(list 'car 'car)
	(list 'cons 'cons)
	(list 'cdr 'cdr)
	(list 'list 'list)
	(list '- '-)
	(list '< '<)
	(list '> '>)
	(list '= '=)
	(list 'pair? 'consp)
	(list 'internal-time-ms 'interpreter-get-internal-time-ms)
	(list 'symbol? 'symbolp)
	(list 'number? 'numberp)
	(list 'string? 'stringp)
	(list 'eval 'schemeval)
	(list 'apply 'interpreter-apply)
	(list 'load 'schemeload)
	(list 'read 'read)
	(list 'write 'write)
	(list 'eq? 'eql)))

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

(defvar *scheme-true-value* t)
(defvar *scheme-false-value* nil)

(defun scheme-true? (val)
  (eql val *scheme-true-value*))

(defun scheme-false? (val)
  (eql val *scheme-false-value*))

(assign-value 'true *scheme-true-value*
	      *the-global-environment*)
(assign-value 'false *scheme-false-value*
	      *the-global-environment*)
(assign-value 'user-initial-environment
	      *the-global-environment*
	      *the-global-environment*)
(assign-value 'load (make-primitive-procedure
		     'schemeload)
	      *the-global-environment*)
