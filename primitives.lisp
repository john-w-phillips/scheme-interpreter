(in-package :scheme-compiler)
(defclass primitive-procedure ()
  ((procedure :initarg :proc :reader primitive-procedure-proc)))

(defmethod print-object ((p primitive-procedure) s)
  (write-string
   (format nil "[primitive-procedure ~a]" (primitive-procedure-proc p))
   s)
  s)

(defun make-primitive-procedure (proc)
  (make-instance 'primitive-procedure :proc proc))

(defun primitive-procedure? (proc)
  (eq (type-of proc) 'primitive-procedure))
