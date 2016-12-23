;;;; symbolic differentiation

(load "symbolic_data/symbolic_data")


(define (sum? x)
  (and (pair? x) (memq '+ x)))

(define (addend s)
  (cond ((null? s) s)
	((not (pair? s)) '())
        ((eq? (car s) '+) '())
	((eq? (cadr s) '+) (car s))
        (else
         (cons (car s) (addend (cdr s))))))

(define (augend s)
  (let ((tail (cdr (memq '+ s))))
    (if (null? (cdr tail))
        (car tail)
        tail)))
             
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))
(define (multiplicand p)
  (let ((tail (cddr p)))
    (if (null? (cdr tail))
        (car tail)
        tail)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))


(deriv '(x + (3 * (x + (y + 2)))) 'x)
(deriv '(x + 3 * (x + y + 2)) 'x)
