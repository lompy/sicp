(define (pascal-rec row col)
  (cond
    ((or (< col 0) (> col row)) 0)
    ((or (= col 0) (= col row)) 1)
    (else (+ (pascal-rec (- row 1) (- col 1)) (pascal-rec (- row 1) col)))))

(pascal-rec 5 2)
