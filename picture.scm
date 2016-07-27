;;;; picture language

;;; (define (up-split painter n)
;;;   (if (= n 0)
;;;       painter
;;;       (let ((smaller (up-split painter (- n 1))))
;;; 	(below painter (beside smaller smaller)))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
	  (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
				  identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
				  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define (split f g)
  (lambda (painter n)
    (if (= n 0)
	painter
	(let ((smaller ((split f g) painter (- n 1))))
	  (f painter (g smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
			   (edge1-frame frame))
	       (scale-vect (ycor-vect v)
			   (edge2-frame frame))))))

(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cadr v))

(define (add-vect v w)
  (make-vect (+ (xcor-vect v) (xcor-vect w))
	     (+ (ycor-vect v) (ycor-vect w))))

(define (sub-vect v w)
  (make-vect (- (xcor-vect v) (xcor-vect w))
	     (- (ycor-vect v) (ycor-vect w))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
	     (* s (ycor-vect v))))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (edge2-frame frame)
  (cddr frame))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
 	((frame-coord-map frame) (start-segment segment))
	((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (make-segment s e)
  (cons s e))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (frame-outline-painter frame)
  (let ((a (make-vect 0 0))
	(b (make-vect 0 1))
	(c (make-vect 1 0))
	(d (make-vect 1 1)))
    ((segments->painter
     (list (make-segment a b)
	   (make-segment a c)
	   (make-segment b d)
	   (make-segment c d))) frame)))

(define (frame-x-painter frame)
  (let ((a (make-vect 0 0))
	(b (make-vect 0 1))
	(c (make-vect 1 0))
	(d (make-vect 1 1)))
    ((segments->painter
     (list (make-segment a d)
	   (make-segment b c))) frame)))

(define (frame-diamond-painter frame)
  (let ((a (make-vect 0.5 0))
	(b (make-vect 1 0.5))
	(c (make-vect 0 0.5))
	(d (make-vect 0.5 1)))
    ((segments->painter
     (list (make-segment a b)
	   (make-segment a c)
	   (make-segment b d)
	   (make-segment c d))) frame)))
