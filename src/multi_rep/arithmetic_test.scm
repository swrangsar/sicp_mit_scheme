(load "multi_rep/arithmetic")

(install-scheme-number-package)
(add (make-scheme-number 3) (make-scheme-number 7))

(install-rational-package)
(div (make-rational 3 7) (make-rational 21 9))

(install-real-package)
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

(raise 3)
(raise (make-scheme-number 4))
(raise (make-rational 5 1))
(raise (make-rational 3 7))
(raise (make-real 3.14))

(mul (make-rational 22 7) (make-complex-from-mag-ang 5 1.57))
(mul 3 (make-complex-from-mag-ang 5 1.57))
(div 3 (make-complex-from-mag-ang 5 1.57))
(add 3 (make-complex-from-mag-ang 5 1.57))
(add 3 (make-rational 22 7))


(raise (make-complex-from-mag-ang 5 3.14))
