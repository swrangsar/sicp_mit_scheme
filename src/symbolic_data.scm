;;;; symbolic data, quotation

(define (memq item x)
  (cond ((null? x) false)
	((eq? item (car x)) x)
	(else (memq item (cdr x)))))

;;; (memq 'apple '(pear banana prune))
;;; (memq 'apple '(x (apple sauce) y apple pear))
;;; 
;;; (list 'a 'b 'c)
;;; (list (list 'george))
;;; (cdr '((x1 x2) (y1 y2)))
;;; 
;;; (cadr '((x1 x2) (y1 y2)))
;;; (pair? (car '(a short list)))
;;; (memq 'red '((red shoes) (blue socks)))
;;; (memq 'red '(red shoes blue socks))

(define (equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b)))
	 (eq? a b))
	((and (pair? a) (pair? b))
	 (and (equal? (car a) (car b))
	      (equal? (cdr a) (cdr b))))
	(else false)))

;;; (equal? '(this is a list) '(this is a list))
;;; (equal? '(this is a list) '(this (is a) list))

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	   (make-product (multiplier exp)
			 (deriv (multiplicand exp) var))
	   (make-product (deriv (multiplier exp) var)
			 (multiplicand exp))))
	((exponentiation? exp)
	 (let ((u (base exp))
	       (n (exponent exp)))
	   (make-product
	    (make-product n
			  (make-exponentiation u (- n 1)))
	    (deriv u var))))
	(else
	 (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;;; (define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
  (let ((tail (cddr s)))
    (if (null? (cdr tail))
	(car tail) 
	(cons '+ tail))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (let ((tail (cddr p)))
    (if (null? (cdr tail))
	(car tail) 
	(cons '* tail))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
	((=number? exponent 1) base)
	(else (list '** base exponent))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e)
	(cadr e))

(define (exponent e)
	(caddr e))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(** (+ x 3) 3) 'x)
(deriv '(* x y (+ x 3)) 'x)
