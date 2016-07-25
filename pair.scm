;;;; pair of non-negative integer as number

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car z)
  (define (iter a result)
    (if (= (remainder a 2) 0)
	(iter (/ a 2) (+ result 1))
	result))
  (iter z 0))

(define (cdr z)
  (define (iter a result)
    (if (= (remainder a 3) 0)
	(iter (/ a 3) (+ result 1))
	result))
  (iter z 0))

(define sampledata (cons 7 17))
(car sampledata)
(cdr sampledata)
