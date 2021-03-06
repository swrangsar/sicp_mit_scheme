;;;; tree structure

(define nil ())
;;; 
;;; (define (scale-tree tree factor)
;;;   (cond ((null? tree) nil)
;;; 	((not (pair? tree)) (* tree factor))
;;; 	(else (cons (scale-tree (car tree) factor)
;;; 		    (scale-tree (cdr tree) factor)))))
;;; 
;;; (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
;;; 	    10)
;;; 
(define (scale-tree tree factor)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (scale-tree sub-tree factor)
	     (* sub-tree factor)))
       tree))

;;; (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
;;; 	    10)
;;; 

;;; (define (square-tree tree)
;;;   (cond ((null? tree) nil)
;;; 	((not (pair? tree)) (square tree))
;;; 	(else (cons (square-tree (car tree))
;;; 		    (square-tree (cdr tree))))))

;;; (define (square-tree tree)
;;;   (map (lambda (sub-tree)
;;; 	 (if (pair? sub-tree)
;;; 	     (square-tree sub-tree)
;;; 	     (square sub-tree)))
;;;        tree))

(define (tree-map f tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map f sub-tree)
	     (f sub-tree)))
       tree))

(define (square-tree tree)
  (tree-map square tree))

;;; (square-tree
;;;  (list 1
;;;        (list 2 (list 3 4) 5)
;;;        (list 6 7)))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (x)
			    (cons (car s) x))
			  rest)))))

;;; (subsets (list 1 2 3))
