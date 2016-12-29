(load "symbolic_data/symbolic_data")
(load "multi_rep/table_2d")
(load "multi_rep/data_directed")


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))



(define (same-variable? v1 v2)
  (and (symbol? v1) (symbol? v2) (eq? v1 v2)))


(define (deriv-sum exp var)
  (make-sum (deriv (addend exp) var)
            (deriv (augend exp) var)))

(define (deriv-product exp var)
  (make-sum
    (make-product (multiplier exp)
                  (deriv (multiplicand exp) var))
    (make-product (deriv (multiplier exp) var)
                  (multiplicand exp))))

(define (addend s) (car s))
(define (augend s)
  (let ((tail (cdr s)))
    (if (null? (cdr tail))
        (car tail)
        (cons '+ tail))))

(define (multiplier p) (car p))
(define (multiplicand p)
  (let ((tail (cdr p)))
    (if (null? (cdr tail))
        (car tail)
        (cons '* tail))))


(define (install-deriv)
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  'done)

(define (deriv-exponentiation expr var)
  (let ((base (base expr))
        (exponent (exponent expr)))
    (make-product exponent
                  (make-product (make-exponentiation base
                                                     (make-sum exponent -1))
                                (deriv base var)))))


(define (base expr) (car expr))
(define (exponent expr) (cadr expr))

(define (install-deriv-exponentiation)
  (put 'deriv '** deriv-exponentiation)
  'done)

(install-deriv)
(install-deriv-exponentiation)


(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(** (+ x 3) 3) 'x)
(deriv '(* x y (+ x 3)) 'x)

;;; (put '+ 'deriv deriv-sum)
;;; (put '* 'deriv deriv-product)
;;; (put '** 'deriv deriv-exponentiation)
