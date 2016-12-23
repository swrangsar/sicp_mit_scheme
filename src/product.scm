(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a)
	      (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (define (inc n) (+ n 1))
  (define (identity x) x)
  (product identity 1 inc n))

(define (pi-approx b)
  (define (pi-next x) (+ x 2))
  (define (pi-ratio b)
    (* 2
       b
       (/ (product square 2 pi-next (- b 2))
          (product square 3 pi-next (- b 1)))))
  (if (even? b)
      (pi-ratio b)
      (pi-ratio (- b 1))))

(pi-approx 31)
