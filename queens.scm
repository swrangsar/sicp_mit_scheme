;;;; eight-queens puzzle for n-queen

(load "sequence")

(define empty-board ())

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (safe? k positions)
  (define (iter col pv)
    (cond ((= col 0) true)
	  ((= (car positions) (car pv)) false)
	  ((= (- k col) (abs (- (car positions) (car pv)))) false)
	  (else (iter (- col 1) (cdr pv)))))
  (iter (- k 1) (cdr positions)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 0)
(queens 1)
(queens 2)
(queens 3)
(queens 4)
(queens 5)
(queens 6)
(queens 7)
(queens 8)
(queens 9)
