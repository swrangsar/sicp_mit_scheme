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

;;; (define (even-fibs n)
;;;   (define (next k)
;;;     (if (> k n)
;;; 	nil
;;; 	(let ((f (fib k)))
;;; 	  (if (even? f)
;;; 	      (cons f (next (+ k 1)))
;;; 	      (next (+ k 1))))))
;;;   (next 0))

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

(define (sum-odd-squares tree)
  (accumulate +
	      0
	      (map square
		   (filter odd?
			   (enumerate-tree tree)))))

;;;(sum-odd-squares sample-tree)

(define (even-fibs n)
  (accumulate cons
	      nil
	      (filter even?
		      (map fib
			   (enumerate-interval 0 n)))))

;;; (even-fibs 13)

(define (list-fib-squares n)
  (accumulate cons
	      nil
	      (map square
		   (map fib
			(enumerate-interval 0 n)))))

;;; (list-fib-squares 10)

(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
	      1
	      (map square
		   (filter odd? sequence))))

;;; (product-of-squares-of-odd-elements (list 1 2 3 4 5))

;;; (define (map p sequence)
;;;   (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

;;; (map square (list 1 3 7))

;;; (define (append seq1 seq2)
;;;   (accumulate cons seq2 seq1))

;;; (append (list 3 7 5) (list 1 2 3))

;;; (define (length sequence)
;;;  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
;;; 
;;; (length (list 2 7 17 5 3))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff (* x higher-terms)))
	      0
	      coefficient-sequence))

;;; (horner-eval 2 (list 1 3 0 5 0 1))

(define (count-leaves t)
  (accumulate (lambda (x y) (+ (length x) y))
	      0
	      (map enumerate-tree t)))

;;; (count-leaves sample-tree)

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init
	     		(map car seqs))
	    (accumulate-n op init
	     		  (map cdr seqs)))))

(accumulate-n +
	      0
	      (list (list 1 2 3)
		    (list 4 5 6)
		    (list 7 8 9)
		    (list 10 11 12)))
