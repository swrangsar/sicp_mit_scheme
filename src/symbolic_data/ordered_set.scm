(define sample-set '(1 3 6 10))


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(element-of-set? 3 sample-set)
(element-of-set? 7 sample-set)


(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))


(intersection-set '(3 4 5 6 7) sample-set)


(define (adjoin-set x set)
  (if (null? set)
      (list x)
      (let ((head (car set)))
        (cond ((= x head) set)
              ((< x head) (cons x set))
              (else (cons head (adjoin-set x (cdr set))))))))

(adjoin-set 7 sample-set)
(adjoin-set 5 sample-set)
(adjoin-set 11 sample-set)


(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1
                        (union-set (cdr set1)
                                   (cdr set2))))
                 ((< x1 x2)
                  (cons x1
                        (union-set (cdr set1) set2)))
                 ((< x2 x1)
                  (cons x2
                        (union-set set1 (cdr set2)))))))))  


(union-set '(2 3 5 7 11) sample-set)
