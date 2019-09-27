(in-package :scheme-compiler)
(defun schemeload (filename)
  (with-open-file
      (filestream filename)
    (read-all-forms filestream nil)))

(defun read-all-forms (filestream last-value)
  (handler-case
      (read-all-forms filestream (read-one-form filestream))
    (end-of-file (var) (declare (ignore var)) last-value)))

(defun read-one-form (filestream)
  (schemeval
   (let ((*package* (find-package :scheme-compiler)))
     (read filestream))
   *the-global-environment*))
