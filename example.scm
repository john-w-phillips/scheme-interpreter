(define (fibo n)
  (if (or (= n 1) (= n 0)) 1
      (+ (fibo (- n 1)) (fibo (- n 2)))))
