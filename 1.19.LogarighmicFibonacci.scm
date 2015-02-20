(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (p-for-squared-transform p q)
  (+ (square p) (square q)))

(define (q-for-squared-transform p q)
  (+ (square q) (* 2 p q)))

(define (fib-iter a b p q count)
  (cond
    ((= count 0) b)
    ((even? count)
     (fib-iter
       a
       b
       (p-for-squared-transform p q)
       (q-for-squared-transform p q)
       (/ count 2)))
    (else 
      (fib-iter 
        (+ (* b q) (* a q) (* a p))
        (+ (* b p) (* a q))
        p
        q
        (- count 1)))))

(fib 10)
