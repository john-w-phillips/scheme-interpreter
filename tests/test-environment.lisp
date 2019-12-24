(in-package :scheme-compiler-tests)

(define-test test-cell
  (let ((cell (make-cell 'x 2)))
    (is
     equal
     (cell-value cell) 2)
    (is equal
     (cell-variable cell) 'x)
    (setf (cell-value cell) 5)
    (is equal
     (cell-value cell) 5)
    (is
     equal
     (cell-variable cell) 'x)))


(define-test test-frames
    (let ((frame (make-frame '(a b) '(1 2))))
      (is
       equals
       (frame-lookup-val frame 'a)
       (make-cell 'a 1))
      (true
       (frame? frame))
      (is
       equals
       (frame-lookup-val frame 'b)
       (make-cell 'b 2))
      (is
       equal
       nil
       (frame-lookup-val frame 'z))))

(define-test test-frame-assignment
  (let ((frame (make-frame '() '())))
    (true
     (not (null
	   (frame-assign-val frame 'x 2))))
    (is
     equals
     (frame-lookup-val frame 'x)
     (make-cell 'x 2))))

(define-test test-environment
  (true
   (empty-environment? the-empty-environment))
  (let* ((new-frame (make-frame '(a) '(2)))
	 (new-environ (extend-environment the-empty-environment new-frame)))
    (is
     equals
     (environment-first-frame
      new-environ)
      new-frame)
    (is
     equals
     (environment-find-cell new-environ 'a)
     (make-cell 'a 2))
    (true
     (empty-environment?
     (enclosing-environment new-environ)))))

