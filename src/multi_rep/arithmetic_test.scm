(load "multi_rep/arithmetic")

(install-scheme-number-package)
(add (make-scheme-number 3) (make-scheme-number 7))

(install-rational-package)
(div (make-rational 3 7) (make-rational 21 9))

(install-rectangular-package)
(install-polar-package)
(install-complex-package)

(mul (make-complex-from-mag-ang 3 1.57) (make-complex-from-mag-ang 5 1.57))
(magnitude (make-complex-from-real-imag 3 4))
(equ? (make-complex-from-mag-ang 3 1.57)
      (make-complex-from-mag-ang 3 1.57))
(=zero? (make-scheme-number 0))
(=zero? (make-scheme-number 0.0))
(=zero? (make-rational 0 11))
(=zero? (make-rational 11 0))
(=zero? (make-complex-from-real-imag 0 0.1))
(=zero? (make-complex-from-real-imag 0 0))
(=zero? (make-complex-from-mag-ang 0 10))
