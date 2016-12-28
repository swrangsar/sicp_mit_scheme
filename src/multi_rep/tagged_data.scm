(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (attach-tag type-tag contents)
  (cond ((eq? type-tag 'scheme-number) contents)
        (else (cons type-tag contents))))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (error "Bad tagged datum -- CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "No method for these types" (list op type-tags))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                              (error "No method for these types"
                                     (list op type-tags)))))))
              (error "No method for these types"
                     (list op type-tags)))))))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (> (length args) 1)
              (let ((final-type (get-coercion-type-tag type-tags)))
                (if final-type
                    (let ((new-args (map (lambda (x) (coerce-type final-type x)) args)))
                      (display "on new args")
                      (apply apply-generic op new-args))
                    (error "No method for these types"
                           (list op type-tags))))
              (error "No method for these types"
                     (list op type-tags)))))))


(define (get-coercion-type-tag type-tags)
  (define (is-coercion-type type types)
    (cond ((null? types) true)
          ((eq? type (car types)) (is-coercion-type type (cdr types)))
          ((get-coercion (car types) type) (is-coercion-type type (cdr types)))
          (else false)))
  (define (coercion-type type-list)
    (cond ((null? type-list) false)
          ((is-coercion-type (car type-list) type-tags) (car type-list))
          (else (coercion-type (cdr type-list)))))
  (cond ((null? type-tags) false)
        ((is-all-same-type type-tags) false)
        (else (coercion-type type-tags))))

(define (coerce-type dest_type datum)
  (let ((src_type (type-tag datum)))
    (if (eq? dest_type src_type)
        datum
        (let ((transform (get-coercion src_type dest_type)))
          (if transform
              (transform datum)
              (error "no coercion for" src_type "to" dest_type))))))

(define (is-all-same-type type-tags)
  (define (comp-types types)
    (cond ((null? types) true)
          ((eq? (car types) (car type-tags)) (comp-types (cdr types)))
          (else false)))
  (comp-types (cdr type-tags)))


(define (try-raising-to arg target-type)
  (let ((type (type-tag arg)))
    (if (eq? type target-type)
        arg
        (let ((raiser (get 'raise (list type))))
          (if (not raiser)
              false
              (try-raising-to (raiser (contents arg)) target-type))))))


(define (get-raised-args args)
  (define (raise-sublist head tail)
    (if (null? tail)
        (error "null tail for raise-sublist")
        (let ((next-head (car tail)))
          (if (eq? (type-tag head) (type-tag next-head))
              (cons head (raise-sublist next-head (cdr tail)))
              (let ((new-arg1 (try-raising-to head (type-tag next-head))))
                   (if new-arg1
                       (cons new-arg1 tail)
                       (let ((new-arg2 (try-raising-to next-head (type-tag head))))
                            (if new-arg2
                                (cons head (cons new-arg2 (cdr tail)))
                                (error "could not coerce args" head next-head)))))))))
  (raise-sublist (car args) (cdr args)))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (> (length args) 1)
              (if (is-all-same-type type-tags)
                  (error "No method for these type" (list op type-tags))
                  (apply apply-generic op (get-raised-args args)))
              (error "No method for these types"
                     (list op type-tags)))))))


(define (drop x)
  (if (pair? x)
      (let ((type (type-tag x)))
        (let ((projecter (get 'project (list type))))
          (if (not projecter)
              x
              (let ((projection (projecter (contents x))))
              (if (equ? x (raise projection))
                  (drop projection)
                  x)))))
      x))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop (apply proc (map contents args)))
          (if (> (length args) 1)
              (if (is-all-same-type type-tags)
                  (error "No method for these type" (list op type-tags))
                  (drop (apply apply-generic op (get-raised-args args))))
              (error "No method for these types"
                     (list op type-tags)))))))
