(define (double a)
  (* a 2))

(define (halve a)
  (/ a 2))

(define (fast-mul a b)
  (cond
    ((= b 0) 0)
    ((even? b) (fast-mul (double a) (halve b)))
    (else (+ a (fast-mul a (- b 1))))))

(fast-mul 10 30)


; Again usin invariant quantity hint.
(define (fast-mul-iter a b)
  (define (inner-iter a b sum)
    (cond
      ((= b 0) sum)
      ((even? b) (inner-iter (double a) (halve b) sum))
      (else (inner-iter a (- b 1) (+ a sum)))))
  (inner-iter a b 0))

(fast-mul-iter 10 30)
