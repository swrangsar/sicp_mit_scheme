(load "multi_rep/arithmetic")

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(define (exp x y) (apply-generic 'exp x y))

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
