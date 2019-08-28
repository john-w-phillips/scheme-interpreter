(in-package :scheme-compiler)
(defun make-primitive-procedure (proc)
  (list 'primitive-procedure proc))
(defun primitive-procedure-proc (proc)
  (cadr proc))

(defun primitive-procedure? (proc)
  (tagged-list? proc 'primitive-procedure))
