(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

; My solution.
; It is not always iterative, 
; space is growing as log_2(n) due to else multiplication being not tailed 
(define (iter-expt b n)
  (define (inner-iter n counter accumulator)
    (cond 
      ((= n 0) 1)
      ((= n 1) accumulator)
      ((<= counter (/ n 2)) (inner-iter n (* counter 2) (square accumulator)))
      (else (* accumulator (inner-iter (- n counter) 1 b)))))
  (inner-iter n 1 b))
    
; With invariant quantity hint.
; It is really constant space iterative process, all recursions are tailed.
(define (iter-e b n)
  (define (inner-iter b n a)
    (define sq-b (square b))
    (cond 
      ((= n 0) a)
      ((even? n) (inner-iter sq-b (/ n 2) a))
      (else (inner-iter b (- n 1) (* b a)))))
  (inner-iter b n 1))
