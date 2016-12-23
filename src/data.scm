;;;; what is data?

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define sampledata (cons 2 3))

(car sampledata)
(cdr sampledata)
