(define (rec-f n)
  (if
    (< n 3)
    n
    (+ (rec-f (- n 1)) (* 2 (rec-f (- n 2))) (* 3 (rec-f (- n 3))))))

(define (iter-f n)
  (define (definition-of-f f-n-1 f-n-2 f-n-3)
    (+ f-n-1 (* 2 f-n-2) (* 3 f-n-3)))
  (define (inner-iter-f counter f-n-1 f-n-2 f-n-3)
    (if
      (= counter 0)
      f-n-1
      (inner-iter-f 
        (- counter 1) 
        (definition-of-f f-n-1 f-n-2 f-n-3)
        f-n-1 
        f-n-2)))
  (if
    (< n 3)
    n
    (inner-iter-f (- n 2) 2 1 0)))


(rec-f  4)
(iter-f 4)
