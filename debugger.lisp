(in-package :scheme-compiler)

(defconstant PRINT-FRAME 1 "Print the current frame")
(defconstant UP 2 "Go one frame up")
(defclass console-option-printer () ())

(defgeneric print-options (printer format &rest options)
  (:documentation "Print options from priter using format and options"))

(defmethod print-options ((printer console-option-printer) &rest opts)
  )
(defclass debugger ()
  ((initial-error
    :initarg :error
    :initform (error "Must have an error value"))
   (current-environment
    :initform
    :reader)))

(defmethod initialize-instance :after ((debugger debugger)
			      &key)
  (with-slots (current-environment initial-error) debugger
    (setf current-environment
	  (slot-value initial-error 'env))))

(defmethod debugger-display-menu ((debugger debugger))
  (format t "Options: ~%")
  (format t ";; => ~a. Print frame~%" PRINT-FRAME)
  (format t ";; => ~a. Up frame~%" UP))

(defmethod debugger-get-option ((debugger debugger))
     (parse-integer (read-line)))

(defun print-a-value (value)
  (cond
    ((primitive-procedure? value)
     (format nil "#[primitive-procedure ~a]" (primitive-procedure-proc value)))
    ((procedure? value)
     (format nil "#[procedure]"))
    (t
     (format nil "~a" value))))

(defmethod print-current-frame ((debugger debugger))
  (with-slots (current-environment) debugger
    (let ((current-frame (environment-first-frame current-environment)))
      (labels ((print-frame (frame-cells)
		 (cond
		   ((null frame-cells) t)
		   (t
		    (let ((cell (car frame-cells)))
		      (format t "~a : ~a~%"
			      (car cell)
			      (print-a-value (cdr cell)))
		      (print-frame (cdr frame-cells)))))))
	(print-frame (frame-define-cells current-frame))))))

(defmethod interact-with-user ((debugger debugger))
  (progn
    (debugger-display-menu debugger)
    (let ((option (debugger-get-option debugger)))
      (cond
	((= option PRINT-FRAME)
	 (print-current-frame debugger)
	 (interact-with-user debugger))
	((= option UP)
	 (go-one-frame-up debugger)
	 (interact-with-user))
	(t (error "Invalid option"))))))
