;;;; symbolic algebra
;;;; polynomials algebraic


;;; (define (adjoin-term term term-list)
;;;   ((get 'adjoin-term (type-tag term-list))
;;;    term
;;;    (contents term-list)))
;;; 
;;; (define (the-empty-termlist)
;;;   ((get 'the-empty-termlist 'sparse-termlist)))
;;; 
;;; (define (first-term term-list) (apply-generic 'first-term term-list))
;;; (define (rest-terms term-list) (apply-generic 'rest-terms term-list))
;;; (define (empty-termlist? term-list) (apply-generic 'empty-termlist? term-list))


(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? v) (symbol? v))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (add L1 L2)
    (apply-generic 'add L1 L2))
    
  ;; representation of terms and term lists
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (sub (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- SUB-POLY"
               (list p1 p2))))
  ;;interface to the rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put '=zero? '(polynomial)
       (lambda (p) (empty-termlist? (term-list p))))
  'done)


(define (make-polynomial-sparse var terms)
  ((get 'make 'polynomial)
   var
   ((get 'make 'sparse-termlist) terms)))

(define (make-polynomial-dense var terms)
  ((get 'make 'polynomial)
   var
   ((get 'make 'dense-termlist) terms)))
