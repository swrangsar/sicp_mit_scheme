;;;; interval arithmetic

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

#|
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))
|#

(define (div-interval x y)
  (let ((yl (lower-bound y))
	(yu (upper-bound y)))
    (if (or (= yl 0) (= yu 0))
	(error "zero bound yl " yl " yu " yu)
	(mul-interval x
	  (make-interval (/ 1.0 yu)
			 (/ 1.0 yl))))))

(define (make-interval a b)
      (cons a b))

(define (upper-bound i)
  (cdr i))

(define (lower-bound i)
  (car i))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((a (lower-bound x))
	(b (upper-bound x))
	(c (lower-bound y))
	(d (upper-bound y)))
    (cond ((and (< b 0) (< d 0))
	    (make-interval (* b d) (* a c)))
	  ((and (< b 0) (< c 0))
	    (make-interval (* a d) (* a c)))
	  ((< b 0)
	    (make-interval (* a d) (* b c)))
	  ((and (< a 0) (< d 0))
	    (make-interval (* b c) (* a c)))
	  ((and (< a 0) (< c 0))
	    (make-interval (min (* a d) (* b c))
			   (max (* a c) (* b d))))
	  ((< a 0)
	    (make-interval (* a d) (* b d)))
	  ((< d 0)
	    (make-interval (* b c) (* a d)))
	  ((< c 0)
	    (make-interval (* b c) (* b d)))
	  (else
	    (make-interval (* a c) (* b d))))))

(define (make-center-percent c p)
  (let ((width (* c (/ p 100))))
    (make-interval (- c width)
		   (+ c width))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (define (calc-pc c l)
    (let ((w (- c l)))
      (* 100 (/ w c))))
  (calc-pc (center i) (lower-bound i)))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))
