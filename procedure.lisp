(in-package :scheme-compiler)
(defclass procedure (analyzed-syntax)
  ((variables :initarg :vars :reader procedure-vars)
   (environment :initarg :env :reader procedure-env)
   (body :initarg :body :reader procedure-body)))

(defmethod initialize-instance :after ((proc procedure) &key)
  (with-slots (body) proc
    (setf
     (slot-value proc 'thunk)
     (sequential-lambda
      (mapcar #'schemeanalyze body)))))

(defun sequential-lambda (procs)
  (lambda (env)
    (let ((result nil))
      (dolist (i procs result) (setq result (execute-syntax i env))))))

(export 'procedure)

(defun procedure? (obj)
  (eq (type-of obj) 'procedure))


(export 'procedure?)


