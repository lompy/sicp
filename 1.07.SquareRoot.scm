(define (avg one two) (/ (+ one two) 2.0))
(define (improve a g) (avg g (/ a g)))

(define (sr1 a p)
  (define (loop g)
    (define (good? new-g) (< (abs (/ (- new-g g) g)) p))
    (define new-g (improve a g))
    (if (good? new-g) new-g (loop new-g)))
  (loop 1))

(define (sr2 a p)
  (define (good? g) (< (abs (- (* g g) a)) p))
  (define (loop g)
    (if (good? g) g (loop (improve a g))))
  (loop 1))

(define n 0.000000000000000023123)

(sqrt n)

(sr1 n 0.00001)

(sr2 n 0.0000000000000000000000001)
