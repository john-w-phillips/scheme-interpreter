(in-package :scheme-compiler-tests)
(define-test test-cell
  (let ((cell (make-cell 'x 2)))
    (assert-equal
     (cell-define-value cell) 2)
    (assert-equal
     (cell-define-variable cell) 'x)
    (cell-assign-value cell 5)
    (assert-equal
     (cell-define-value cell) 5)
    (assert-true
     (cell-define-variable cell) 'x)))

(define-test test-frames
    (let ((frame (make-frame '(a b) '(1 2))))
      (assert-equalp
       (frame-lookup-val frame 'a)
       (make-cell 'a 1))
      (assert-true
       (frame? frame))
      (assert-equalp
       (frame-lookup-val frame 'b)
       (make-cell 'b 2))
      (assert-equal
       nil
       (frame-lookup-val frame 'z))))

(define-test test-frame-assignment
  (let ((frame (make-frame '() '())))
    (assert-true
     (not (null
	   (frame-assign-val frame 'x 2))))
    (assert-equalp
     (frame-lookup-val frame 'x)
     (make-cell 'x 2))))

(define-test test-environment
  (assert-true
   (empty-environment? the-empty-environment))
  (let* ((new-frame (make-frame '(a) '(2)))
	 (new-environ (extend-environment the-empty-environment new-frame)))
    (assert-equalp
     (environment-first-frame
      new-environ)
      new-frame)
    (assert-equalp
     (environment-find-cell new-environ 'a)
     (make-cell 'a 2))
    (assert-true
     (empty-environment?
     (enclosing-environment new-environ)))))

