;;;; list

(define squares (list 1 4 9 16 25))
(define odds (list 1 3 5 7))

;;; (define (list-ref items n)
;;;   (if (= n 0)
;;;       (car items)
;;;       (list-ref (cdr items) (- n 1))))
;;; 
;;; (list-ref squares 3)
;;; 
;;; (define (length items)
;;;   (if (null? items)
;;;       0
;;;       (+ 1 (length (cdr items)))))
;;; 
;;; (length odds)
;;; 
;;; (define (length items)
;;;   (define (length-iter a count)
;;;     (if (null? a)
;;; 	count
;;; 	(length-iter (cdr a) (+ 1 count))))
;;;   (length-iter items 0))
;;; 
;;; (length squares)
;;; 
;;; (define (append list1 list2)
;;;   (if (null? list1)
;;;       list2
;;;       (cons (car list1) (append (cdr list1) list2))))
;;; 
;;; (append squares odds)
;;; (append odds squares)

(define (last-pair l)
  (define (iter v)
    (let ((next (cdr v)))
      (if (null? next)
	  v
	  (iter next))))
  (if (null? l)
      l
      (iter l)))

;;; (last-pair (list 23 72 149 34))
;;; 
;;; (define (reverse lst)
;;;   (define (iter l result)
;;;     (if (null? l)
;;; 	result
;;; 	(iter (cdr l) (cons (car l) result))))
;;;   (iter lst ()))
;;; 
;;; (reverse (list 1 4 9 16 25))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else
	  (+ (cc amount
		 (except-first-denomination coin-values))
	     (cc (- amount
		   (first-denomination coin-values))
		coin-values)))))

(define (first-denomination v)
  (car v))

(define (except-first-denomination v)
  (cdr v))

(define (no-more? v)
  (null? v))

;;; (cc 100 us-coins)
;;; (cc 100 (reverse us-coins))

(define (same-parity a . l)
  (define (iter v result)
    (if (null? v)
	(reverse result)
	(let ((item (car v)))
	  (if (= (remainder (- item a) 2) 0)
	      (iter (cdr v) (cons item result))
	      (iter (cdr v) result)))))
  (iter l (cons a ())))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
