;;;; sequence as conventional interface

(load "prime")

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

;;; (accumulate-n +
;;; 	      0
;;; 	      (list (list 1 2 3)
;;; 		    (list 4 5 6)
;;; 		    (list 7 8 9)
;;; 		    (list 10 11 12)))

(define sample-vector (list 1 2 3 4))
(define sample-matrix (list (list 1 2 3 4)
			    (list 4 5 6 6)
			    (list 6 7 8 9)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (r)
	 (dot-product r v))
       m))

;;; (dot-product sample-vector sample-vector)
;;; (matrix-*-vector sample-matrix sample-vector)

(define (transpose mat)
  (accumulate-n cons nil mat))

;;; (transpose sample-matrix)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (r)
	   (matrix-*-vector cols r))
	 m)))

;;; (matrix-*-matrix sample-matrix
;;; 	 	 (transpose sample-matrix))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)
;;; 
;;; (fold-right / 1 (list 1 2 3))
;;; (fold-left / 1 (list 1 2 3))
;;; (fold-right list nil (list 1 2 3))
;;; (fold-left list nil (list 1 2 3))
;;; (fold-right * 1 (list 1 2 3))
;;; (fold-left * 1 (list 1 2 3))
;;; (fold-right + 1 (list 1 2 3))
;;; (fold-left + 1 (list 1 2 3))
;;; 
;;; (fold-right - 1 (list 1 2 3))
;;; (fold-left - 1 (list 1 2 3))

(define (reverse sequence)
  (fold-right (lambda (x y)
		(append y (list x)))
	      nil
	      sequence))

;;; (reverse sample-vector)

(define (reverse sequence)
  (fold-left (lambda (x y)
	       (cons y x))
	     nil
	     sequence))

;;; (reverse sample-vector)
;;; (prime? 71)
;;; (prime? 117)

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (flatmap
		(lambda (i)
		  (map (lambda (j) (list i j))
		       (enumerate-interval 1 (- i 1))))
		(enumerate-interval 1 n)))))

;;; (prime-sum-pairs 6)

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
	  sequence))

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
		 (map (lambda (p) (cons x p))
		      (permutations (remove x s))))
	       s)))

;;; (remove 7 (list 1 17 37 73 7 10 19))
;;; (permutations (list 1 2 3))
;;; (permutations (list 1 2 3 4))
