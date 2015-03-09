;#lang planet neil/sicp

(define (square x) (* x x))

(define (next x) (if (= x 2) (+ x 3) (+ x 2)))
;(define (next x) (+ x 1))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (next test-divisor)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if
    (fast-prime? n 100)
    (report-prime n (- (runtime) start-time))
    false))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time)
  true)

(define (report-not-prime elapsed-time)
  (display " --- ")
  (display elapsed-time)
  false)

(define (search-for-primes range-start primes-needed)
  (cond
    ((= 0 primes-needed) (display " end "))
    ((even? range-start) (search-for-primes (+ 1 range-start) primes-needed))
    ((timed-prime-test range-start) (search-for-primes (+ 2 range-start) (- primes-needed 1)))
    (else (search-for-primes (+ 2 range-start) primes-needed))))

(define (expmod base exp m)
  (cond
    ((= exp 0) 1)
    ((even? exp)
     (remainder
       (square (expmod base (/ exp 2) m))
       m))
    (else
      (remainder
        (* base (expmod base (- exp 1) m))
        m))))

;(define (expmod base exp m) (remainder (fast-exp base exp) m))
(define (fast-exp b n)
  (define (inner-iter b n a)
    (define sq-b (square b))
    (cond 
      ((= n 0) a)
      ((even? n) (inner-iter sq-b (/ n 2) a))
      (else (inner-iter b (- n 1) (* b a)))))
  (inner-iter b n 1))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond
    ((= times 0) true)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else false)))

(timed-prime-test 19)
(timed-prime-test 199)
(timed-prime-test 1999)
(timed-prime-test 19999)
(timed-prime-test 199999)
(timed-prime-test 1999999)
(timed-prime-test 19999999)

(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)
(search-for-primes 1000000 3)
(search-for-primes 10000000 3)
(search-for-primes 100000000 3)
