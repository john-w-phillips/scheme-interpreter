(in-package :scheme-compiler)

(defconstant PRINT-FRAME 1 "Print the current frame")
(defconstant UP 2 "Go one frame up")
(defconstant QUIT 3 "Quit debugger")

(defclass menu-option ()
  ((option-name :initarg :name :initform (error "Must have option name") :reader option-name)
   (option-key :initarg :key :initform (error "Must have option key") :reader option-key)
   (option-possible-values :initarg :values)))
(defclass view-output ()
  ((string-value :initarg :string :reader string-value)))
(defclass viewer () ())
(defclass console-menu-view (viewer) ())

(defgeneric view-options (view title &rest options)
  (:documentation "Print options from viewer"))
(defgeneric view-get-key-input (view)
  (:documentation "Get a key input for a previous option viewing"))
(defgeneric view-send-results (view &rest outputs)
  (:documentation "Print outputs from viewer"))
(defmethod view-options ((view console-menu-view) title &rest options)
  (format t "~a~%" title)
  (dolist (option options)
    (format t "~a: ~a~%" (option-name option) (option-key option))))

(defmethod view-get-key-input ((view console-menu-view))
  (format t "Please press a key...")
  (let ((num (parse-integer (read-line))))
    num))

(defmethod view-send-results ((view console-menu-view) &rest outputs)
  (dolist
      (output outputs)
    (format t "~a~%" (string-value output))))

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

(defmethod debugger-display-menu ((debugger debugger) (viewer viewer))
  (view-options viewer
		"Debug options"
		(make-instance
		 'menu-option
		 :name "Print frame"
		 :key PRINT-FRAME)
		(make-instance
		 'menu-option
		 :name "Up frame"
		 :key UP)
		(make-instance
		 'menu-option
		 :name "Quit"
		 :key QUIT)))

(defun print-a-value (value)
  (cond
    ((primitive-procedure? value)
     (format nil "#[primitive-procedure ~a]" (primitive-procedure-proc value)))
    ((procedure? value)
     (format nil "#[procedure]"))
    (t
     (format nil "~a" value))))

(defmethod go-one-frame-up ((debugger debugger))
  (with-slots (current-environment) debugger
    (setq current-environment (enclosing-environment current-environment))))
 
(defmethod print-current-frame ((debugger debugger) (view viewer))
  (with-slots (current-environment) debugger
    (if (null current-environment)
	nil
	(let ((current-frame (environment-first-frame current-environment)))
	  (labels ((print-frame (frame-cells)
		     (cond
		       ((null frame-cells) t)
		       (t
			(let ((cell (car frame-cells)))
			  (view-send-results
			   view
			   (make-instance
			    'view-output
			    :string (format nil
					    "~a : ~a~%"
					    (car cell)
					    (print-a-value (cdr cell))))))
			(print-frame (cdr frame-cells))))))
	    (print-frame (frame-define-cells current-frame)))))))
(defclass controller () ())

(defgeneric interact-with-user (control view debugger)
  (:documentation "Controller interacts with user"))

(defmethod interact-with-user ((control controller)
			       (view viewer)
			       (debugger debugger))
  (progn
    (debugger-display-menu debugger view)
    (let ((option (view-get-key-input view)))
      (cond
	((equalp option PRINT-FRAME)
	 (print-current-frame debugger view)
	 (interact-with-user control view debugger))
	((equalp option UP)
	 (go-one-frame-up debugger)
	 (interact-with-user control view debugger))
	((equalp option QUIT) 'ok)
	(t (error "Invalid option"))))))

;; (defmethod interact-with-user ((debugger debugger))
;;   (progn
;;     (debugger-display-menu debugger)
;;     (let ((option (debugger-get-option debugger)))
;;       (cond
;; 	((= option PRINT-FRAME)
;; 	 (print-current-frame debugger)
;; 	 (interact-with-user debugger))
;; 	((= option UP)
;; 	 (go-one-frame-up debugger)
;; 	 (interact-with-user))
;; 	(t (error "Invalid option"))))))

(defun console-debugger-debug (an-error)
  (let ((debugger (make-instance 'debugger
				 :error an-error))
	(view (make-instance 'console-menu-view))
	(control (make-instance 'controller)))
    (interact-with-user control view debugger)))

