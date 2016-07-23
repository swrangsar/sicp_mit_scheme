;;;; general methods

(define (search f neg-point pos-point)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.001))
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
	midpoint
	(let ((test-value (f midpoint)))
	  (cond ((> test-value 0)
		 (search f neg-point midpoint))
		((< test-value 0)
		 (search f midpoint pos-point))
		(else midpoint))))))

(define (average x y)
  (/ (+ x y) 2))

;;; (define (close-enough? x y)
;;;   (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (< a-value 0) (> b-value 0))
	   (search f a b))
	  ((and (< b-value 0) (> a-value 0))
	   (search f b a))
	  (else
	   (error "values are not of opposite sign" a b)))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;;; (define (golden-ratio)
;;;  (fixed-point (lambda (y) (average y (+ 1 (/ 1 y))))
;;;	       1.0))

(define (cont-frac n d k)
  (define (recur i)
    (if (> i k)
	0
	(/ (n i)
	   (+ (d i)
	      (recur (+ i 1))))))
  (recur 1))

(define (cont-frac n d k)
  (define (iter k result)
    (if (= k 0)
	result
	(iter (- k 1)
	      (/ (n k)
		 (+ (d k) result)))))
  (iter k 0))

;;; (cont-frac (lambda (i) 1.0)
;;;	       (lambda (i) 1.0)
;;;	        11)


(define (euler-cont-frac n)
  (define (denom k)
    (let ((m (- k 2)))
      (cond ((< k 3) k)
	    ((= (remainder m 3) 0) (* 2 (+ 1 (/ m 3))))
	    (else 1))))
  (cont-frac (lambda (i) 1.0)
	     denom
	     n))

;;; (euler-cont-frac 9)

(define (tan-cf x k)
  (let ((neg-sq (- 0 (square x))))
    (/ x
       (+ 1 (cont-frac (lambda (i) neg-sq)
		       (lambda (i) (+ (* 2 i) 1))
		       k)))))

(define (average-damp f)
  (lambda (x) (average x (f x))))

#|
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
	       1.0))
|#

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
	       1.0))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (cube x) (* x x x))

;;; ((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

#|
(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
		  1.0))
|#

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

#|
(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
			    average-damp
			    1.0))
(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
			    newton-transform
			    1.0))
|#

(define (cubic a b c)
  (lambda (x)
    (+ (* (+ (* (+ x a) x) b) x) c)))

(newtons-method (cubic 2 3 5) 1)
