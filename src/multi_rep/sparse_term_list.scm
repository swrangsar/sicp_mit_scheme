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
  'done)
