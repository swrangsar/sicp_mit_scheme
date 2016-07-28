;;;; symbolic data, quotation

(define (memq item x)
  (cond ((null? x) false)
	((eq? item (car x)) x)
	(else (memq item (cdr x)))))

;;; (memq 'apple '(pear banana prune))
;;; (memq 'apple '(x (apple sauce) y apple pear))
;;; 
;;; (list 'a 'b 'c)
;;; (list (list 'george))
;;; (cdr '((x1 x2) (y1 y2)))
;;; 
;;; (cadr '((x1 x2) (y1 y2)))
;;; (pair? (car '(a short list)))
;;; (memq 'red '((red shoes) (blue socks)))
;;; (memq 'red '(red shoes blue socks))

(define (equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b)))
	 (eq? a b))
	((and (pair? a) (pair? b))
	 (and (equal? (car a) (car b))
	      (equal? (cdr a) (cdr b))))
	(else false)))

;;; (equal? '(this is a list) '(this is a list))
;;; (equal? '(this is a list) '(this (is a) list))
