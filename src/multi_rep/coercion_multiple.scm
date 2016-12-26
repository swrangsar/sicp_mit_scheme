(load "multi_rep/arithmetic")

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (> (length args) 1)
              (let ((final-type (get-coercion-type-tag type-tags)))
                (if final-type
                    (let ((new-args (map (lambda (x) (coerce-type final-type x)) args)))
                      (display "on new args")
                      (apply apply-generic op new-args))
                    (error "No method for these types"
                           (list op type-tags))))
              (error "No method for these types"
                     (list op type-tags)))))))


(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(define (exp x y) (apply-generic 'exp x y))


(define (get-coercion-type-tag type-tags)
  (define (is-coercion-type type types)
    (cond ((null? types) true)
          ((eq? type (car types)) (is-coercion-type type (cdr types)))
          ((get-coercion (car types) type) (is-coercion-type type (cdr types)))
          (else false)))
  (define (coercion-type type-list)
    (cond ((null? type-list) false)
          ((is-coercion-type (car type-list) type-tags) (car type-list))
          (else (coercion-type (cdr type-list)))))
  (cond ((null? type-tags) false)
        ((is-all-same-type type-tags) false)
        (else (coercion-type type-tags))))

(define (coerce-type dest_type datum)
  (let ((src_type (type-tag datum)))
    (if (eq? dest_type src_type)
        datum
        (let ((transform (get-coercion src_type dest_type)))
          (if transform
              (transform datum)
              (error "no coercion for" src_type "to" dest_type))))))


(define (is-all-same-type type-tags)
  (define (comp-types types)
    (cond ((null? types) true)
          ((eq? (car types) (car type-tags)) (comp-types (cdr types)))
          (else false)))
  (comp-types (cdr type-tags)))

(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

(exp 3 2)

(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

(add (make-complex-from-real-imag 3 4) (make-complex-from-real-imag 5 7))
(mul (make-complex-from-real-imag 3 4) (make-complex-from-real-imag 5 7))
(exp (make-complex-from-real-imag 3 4) (make-complex-from-real-imag 5 7))
