;;;; rational number data abstraction

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d)
  (define (normalize a b)
     (if (< b 0)
	 (cons (- a) (abs b))
	 (cons a b)))
  (let ((g (gcd n d)))
    (normalize (/ n g) (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;;; (define one-half (make-rat 1 2))
;;; 
;;; (print-rat one-half)
;;; 
;;; (define one-third (make-rat 1 3))
;;; 
;;; (print-rat (add-rat one-half one-third))
;;; (print-rat (mul-rat one-half one-third))
;;; (print-rat (add-rat one-third one-third))
;;; (print-rat (make-rat -1 2))
;;; (print-rat (make-rat 1 -5))

(define (make-segment start end)
  (cons start end))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (make-point x y) (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (midpoint-segment seg)
  (let ((start (start-segment seg))
	(end (end-segment seg)))
    (make-point
      (average (x-point start) (y-point start))
      (average (x-point end) (y-point end)))))

(define (average x y)
  (/ (+ x y) 2.0))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;;; (print-point
;;;   (midpoint-segment
;;;     (make-segment
;;;       (make-point 0 1)
;;;       (make-point 1 0))))

(define (make-rect a b c d)
  (cons (make-segment a b)
	(make-segment c d)))

(define (distance x y)
  (sqrt (+ (square (- (x-point x) (x-point y)))
	   (square (- (y-point x) (y-point y))))))

(define (rect-width rect)
  (let ((seg (car rect)))
    (distance (start-segment seg)
	      (end-segment seg))))

(define (rect-height rect)
  (let ((x (end-segment (car rect)))
	(y (start-segment (cdr rect))))
    (distance x y)))

(define (rect-perimeter rect)
  (* 2 (+ (rect-width rect)
	  (rect-height rect))))

(define (rect-area rect)
  (* (rect-width rect)
     (rect-height rect)))

(define myrect
  (make-rect (make-point 0 1)
	     (make-point 7 1)
	     (make-point 7 0)
	     (make-point 0 0)))

(rect-perimeter myrect)
(rect-area myrect)
