;;;; dense termlist for polynomials


(define (install-dense-terms-package)
  ;; internal procedures
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (the-empty-termlist) '())
  (define (first-term term-list)
    (let ((num-terms (length term-list)))
      (cond ((= num-terms 1)
             (make-term 0 (car term-list)))
            ((> num-terms 1)
             (if (= (car term-list) 0)
                 (first-term (cdr term-list))
                 (make-term (+ num-terms -1)
                            (car term-list))))
            (else
             (error "empty term-list -- FIRST-TERM DENSE")))))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (let ((len (length term-list)))
          (cond ((< len (order term))
                 (adjoin-term term (cons 0 term-list)))
                ((> len (order term))
                 (error "adjoin term to terms" term))
                (else
                 (cons (coeff term) term-list))))))
  ;; interface to the rest of the system
  (define (tag tl) (attach-tag 'dense-termlist tl))
  (put 'the-empty-termlist '(dense-termlist)
       (lambda () (tag (the-empty-termlist))))
  (put 'empty-termlist? '(dense-termlist)
       (lambda (l) (empty-termlist? l)))
  (put 'first-term '(dense-termlist)
       (lambda (l) (first-term l)))
  (put 'rest-terms '(dense-termlist)
       (lambda (l) (tag (rest-terms l))))
  (put 'adjoin-term 'dense-termlist
       (lambda (t l) (tag (adjoin-term t l))))
  (put 'make 'dense-termlist
       (lambda (l) (tag l)))
  'done)
