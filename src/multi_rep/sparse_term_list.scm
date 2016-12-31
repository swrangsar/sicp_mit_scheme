;;;; sparse term-list for polynomials algebraic

(define (install-sparse-terms-package)
  ;; internal procedures
  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
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
  ;;interface to the rest of the system
  (define (tag l) (attach-tag 'sparse-termlist l))
  (put 'first-term '(sparse-termlist)
       (lambda (l) (first-term l)))
  (put 'rest-terms '(sparse-termlist)
       (lambda (l) (tag (rest-terms l))))
  (put 'adjoin-term 'sparse-termlist
       (lambda (t l) (tag (adjoin-term t l))))
  (put 'empty-termlist? '(sparse-termlist)
       (lambda (l) (empty-termlist? l)))
  (put 'the-empty-termlist 'sparse-termlist
       (lambda () (tag (the-empty-termlist))))
  (put 'make 'sparse-termlist
       (lambda (l) (tag l)))
  (put 'add '(sparse-termlist sparse-termlist)
       (lambda (L1 L2) (tag (add-terms L1 L2))))
  (put 'sub '(sparse-termlist sparse-termlist)
       (lambda (L1 L2) (tag (sub-terms L1 L2))))
  'done)
