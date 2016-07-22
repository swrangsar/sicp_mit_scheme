(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a)
	      (combiner (term a)
			result))))
  (iter a null-value))

(define (filtered-accumulate fltr combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
	  ((fltr a) (iter (next a)
			  (combiner (term a)
				    result)))
	  (else (iter (next a)
		      result))))
  (iter a null-value))


(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (coprime-product n)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (define (coprime? a)
    (= (gcd n a) 1))
  (filtered-accumulate coprime? * 1 identity 1 inc n))
