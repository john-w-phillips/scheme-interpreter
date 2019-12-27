(define (fibo n)
  (if (or (= n 1) (= n 2))
      1
      (+ (fibo (- n 1)) (fibo (- n 2)))))
