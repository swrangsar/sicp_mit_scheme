(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (arctan x y) (apply-generic 'arctan x y))
(define (square x) (mul x x))
(define (square_root x) (apply-generic 'square_root x))
(define (neg x) (apply-generic 'neg x))


(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y))) 
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  (put 'raise '(scheme-number)
       (lambda (x)
         ((get 'make 'rational) x 1)))
  (put 'sine '(scheme-number)
       (lambda (x) (sin x)))
  (put 'cosine '(scheme-number)
       (lambda (x) (cos x)))
  (put 'arctan '(scheme-number scheme-number)
       (lambda (x y) (atan x y)))
  (put 'square_root '(scheme-number)
       (lambda (x) (sqrt x)))
  (put 'neg '(scheme-number)
       (lambda (x) (- x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-integer-package)
  ;; internal procedures
  (define (make-int x)
    (round x))
  (define (neg-int x)
    (- x))
  ;; interface to the rest of the system
  (define (tag x)
    (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(integer integer)
       (lambda (x y) (= x y)))
  (put '=zero? '(integer)
       (lambda (x) (= x 0)))
  (put 'make 'integer
       (lambda (x) (tag (make-int x))))
  (put 'exp '(integer integer)
       (lambda (x y) (tag (expt x y))))
  (put 'raise '(integer)
       (lambda (x)
         ((get 'make 'rational) x 1)))
  (put 'sine '(integer)
       (lambda (x) (sin x)))
  (put 'cosine '(integer)
       (lambda (x) (cos x)))
  (put 'arctan '(integer integer)
       (lambda (x y) (atan x y)))
  (put 'square_root '(integer)
       (lambda (x) (sqrt x)))
  (put 'neg '(integer)
       (lambda (x) (tag (neg-int x))))
  'done)

(define (make-integer n)
  ((get 'make 'integer) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
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
  (define (equ? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (=zero? x)
    (and (= (numer x) 0)))
  (define (project x)
    ((get 'make 'integer) (round (/ (numer x) (denom x)))))
  (define (arctan x y)
    (let ((a (/ (numer x) (denom x)))
          (b (/ (numer y) (denom y))))
      (atan a b)))
  (define (square_root x)
    (sqrt (/ (numer x) (denom x))))
  (define (neg-rat q)
    (make-rat (- (numer q)) (denom q)))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational) =zero?)
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'raise '(rational)
       (lambda (r)
           ((get 'make 'real) (/ (numer r) (denom r)))))
  (put 'project '(rational) project) 
  (put 'sine '(rational)
       (lambda (r) (sin (/ (numer r) (denom r)))))
  (put 'cosine '(rational)
       (lambda (r) (cos (/ (numer r) (denom r)))))
  (put 'arctan '(rational rational) arctan)
  (put 'square_root '(rational) square_root)
  (put 'neg '(rational)
       (lambda (q) (tag (neg-rat q))))
  'done)


(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (make-real r)
  ((get 'make 'real) r))


(define (install-real-package)
  ;; internal procedures
  (define (get-multiplier r)
    (define (calc-multiplier x result)
      (cond ((integer? x) result)
            ((> result 1e9) result)
            (else (calc-multiplier (* 10 x) (* 10 result)))))
    (calc-multiplier r 1))
  (define (project r)
    (let ((denom (get-multiplier r)))
      (let ((numer (round (* r denom))))
        ((get 'make 'rational) numer denom))))
  (define (div x y) (/ x y))
  (define (neg-real x)
    (- x))
  ;; interface to the rest of the system
  (define (tag r) (attach-tag 'real r))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'make 'real
       (lambda (r) (tag r)))
  (put 'raise '(real)
       (lambda (r)
         ((get 'make-from-real-imag 'complex) r 0)))
  (put 'equ? '(real real)
       (lambda (r1 r2) (= r1 r2)))
  (put '=zero? '(real)
       (lambda (r) (= 0 r)))
  (put 'div '(real real) div)
  (put 'project '(real) project)
  (put 'sine '(real)
       (lambda (r) (sin r)))
  (put 'cosine '(real)
       (lambda (r) (cos r)))
  (put 'arctan '(real real)
       (lambda (x y) (atan x y)))
  (put 'square_root '(real)
       (lambda (r) (sqrt r)))
  (put 'neg '(real)
       (lambda (r) (tag (neg-real r))))
  'done)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  (define (equ-complex? z1 z2)
    (and (equ? (magnitude z1) (magnitude z2))
         (equ? (angle z1) (angle z2))
         (equ? (real-part z1) (real-part z2))
         (equ? (imag-part z1) (imag-part z2))))
  (define (=zero-complex? z)
    (=zero? (magnitude z)))
  (define (project z)
    ((get 'make 'real) (real-part z)))
  (define (neg-complex z)
    (sub-complex (make-from-real-imag 0 0)
                 z))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex) equ-complex?)
  (put '=zero? '(complex) =zero-complex?)
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'project '(complex) project)
  (put 'neg '(complex)
       (lambda (z) (tag (neg-complex z))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
