(load "symbolic_data/symbolic_data")
(load "multiple_representation/table_2d")


(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))


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

(define (install-deriv-exponentiation)
  (put 'deriv '** deriv-exponentiation)
  'done)

(install-deriv)
(install-deriv-exponentiation)

(deriv '(* (* x y) (+ x 3)) 'x)

;;; (put '+ 'deriv deriv-sum)
;;; (put '* 'deriv deriv-product)
;;; (put '** 'deriv deriv-exponentiation)
