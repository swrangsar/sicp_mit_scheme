;;;; sequence as conventional interface

(define nil ())
(define sample-tree
  (list 1
	(list 2 (list 3 4) 5)
	(list 6 7)))


(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
	b
	(fib-iter (+ a b)
		  a
		  (- count 1))))
  (fib-iter 1 0 n))

;;; (map fib (list 0 1 2 3 4 5 6 7))

;;; (define (sum-odd-squares tree)
;;;   (cond ((null? tree) 0)
;;; 	((not (pair? tree))
;;; 	 (if (odd? tree)
;;; 	     (square tree)
;;; 	     0))
;;; 	(else (+ (sum-odd-squares (car tree))
;;; 		 (sum-odd-squares (cdr tree))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
	nil
	(let ((f (fib k)))
	  (if (even? f)
	      (cons f (next (+ k 1)))
	      (next (+ k 1))))))
  (next 0))

;;; (sum-odd-squares sample-tree)
;;; (even-fibs 11)

;;; (define (filter predicate sequence)
;;;   (cond ((null? sequence) nil)
;;; 	((predicate (car sequence))
;;; 	 (cons (car sequence)
;;; 	       (filter predicate (cdr sequence))))
;;; 	(else (filter predicate (cdr sequence)))))
;;; 
;;; (filter odd? (list 1 2 3 4 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

;;; (accumulate + 0 (list 1 2 3 4 5))
;;; (accumulate * 1 (list 1 2 3 4 5))
;;; (accumulate cons nil (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

;;; (enumerate-interval 2 7)

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))

;;; (enumerate-tree sample-tree)
