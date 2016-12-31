;;;; dense termlist for polynomials

(define (install-dense-terms-package)
  ;; internal procedures
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (the-empty-termlist) '())
  (define (first-term term-list)
    (make-term (+ (length term-list) -1)
               (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (let ((len (length term-list)))
          (cond ((< len (order term))
                 (adjoin-term term (cons 0 term-list)))
                ((> len (order term))
                 (error "adjoin term to terms" term term-list))
                (else
                 (cons (coeff term) term-list))))))
  (define (make-sparse term-list)
    (let ((empty-sparse-list (get 'the-empty-termlist 'sparse-termlist)))
      (cond ((empty-termlist? term-list) (contents (empty-sparse-list)))
            (else
             (contents ((get 'adjoin-term 'sparse-termlist)
	                (first-term term-list)
                        (make-sparse (rest-terms term-list))))))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  (define (sub-terms L1 L2)
    (cond ((empty-termlist? L2) L1)
          ((empty-termlist? L1)
           (let ((t (first-term L2)))
             (adjoin-term
              (make-term (order t)
                         (neg (coeff t)))
              (sub-terms L1 (rest-terms L2)))))
          (else
            (let ((t1 (first-term L1))
                  (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term
                      t1 (sub-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term
                      (make-term (order t2)
                                 (neg (coeff t2)))
                      (sub-terms L1 (rest-terms L2))))
                    (else
                     (adjoin-term
                      (make-term (order t1)
                                 (sub (coeff t1) (coeff t2)))
                      (sub-terms (rest-terms L1)
                                 (rest-terms L2)))))))))
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
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                       (div-terms
                        (sub-terms L1
                                   (mul-term-by-all-terms
                                    (make-term new-o new-c) L2))
                        L2)))
                  (list (adjoin-term (make-term new-o new-c)
                              (car rest-of-result))
                        (cadr rest-of-result))))))))
  ;; interface to the rest of the system
  (define (tag tl) (attach-tag 'dense-termlist tl))
  (put 'the-empty-termlist 'dense-termlist
       (lambda () (tag (the-empty-termlist))))
  (put 'make 'dense-termlist
       (lambda (l) (tag l)))
  (put 'raise '(dense-termlist)
       (lambda (l) ((get 'make 'sparse-termlist) (make-sparse l))))
  (put 'add '(dense-termlist dense-termlist)
       (lambda (l1 l2) (tag (add-terms l1 l2))))
  (put 'sub '(dense-termlist dense-termlist)
       (lambda (l1 l2) (tag (sub-terms l1 l2))))
  (put 'mul '(dense-termlist dense-termlist)
       (lambda (l1 l2) (tag (mul-terms l1 l2))))
  (put 'div '(dense-termlist dense-termlist)
       (lambda (L1 L2)
         (let ((result (div-terms L1 L2)))
           (list (tag (car result))
                 (tag (cadr result))))))
  'done)
