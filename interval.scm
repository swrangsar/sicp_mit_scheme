;;;; interval arithmetic

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (div-interval x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))

(define (make-interval a b)
  (if (< b a)
      (cons b a)
      (cons a b)))

(define (upper-bound i)
  (cdr i))

(define (lower-bound i)
  (car i))
