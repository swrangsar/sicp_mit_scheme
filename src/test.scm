;;;; Integral using simpson's rule

(define (simpson-integral term a b n)
  (define (simpson-factor k)
    (cond ((= k 0) 1)
	  ((= k n) 1)
	  ((even? k) 2)
	  (else 4)))
  (define (simpson-iter k input h)
    (if (> k n)
	0
	(+ (* (simpson-factor k) (term input))
	   (simpson-iter (+ k 1) (+ input h) h))))
  (define (simpson-sum h)
    (* (/ h 3)
       (simpson-iter 0 a h)))
  (simpson-sum (/ (- b a) n)))

(define (cube x) (* x x x))
(define (inc n) (+ n 1))

