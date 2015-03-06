(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if
    (prime? n)
    (report-prime (- (runtime) start-time))
    (report-not-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (report-not-prime elapsed-time)
  (display " --- ")
  (display elapsed-time))

(define (search-for-primes range-start range-end)
  (if
    (> range-start range-end) 
    (display " end ")
    (cond
      ((even? range-start) (search-for-primes (+ 1 range-start) range-end))
      (else 
        (if (prime? range-start) (display range-start))
        (search-for-primes (+ 2 range-start) range-end)))))

;(timed-prime-test 199)
;(timed-prime-test 1999)
;(timed-prime-test 19999)
;(timed-prime-test 199999)

(search-for-primes 1000 1500)
(search-for-primes 10000 15000)
