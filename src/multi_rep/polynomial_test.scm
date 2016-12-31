(load "multi_rep/table_2d")
(load "multi_rep/tagged_data")
(load "multi_rep/data_directed")
(load "multi_rep/arithmetic")
(load "multi_rep/polynomial")
(load "multi_rep/sparse_term_list")
(load "multi_rep/dense_term_list")


(install-scheme-number-package)
(install-sparse-terms-package)
(install-dense-terms-package)
(install-polynomial-package)


(define poly-a (make-polynomial-sparse 'x '((5 1) (4 2) (2 3) (1 -2) (0 -5))))
(define poly-b (make-polynomial-sparse 'x '((100 1) (2 2) (0 1))))

(add poly-a poly-b)
(sub poly-b poly-a)
(mul poly-a poly-b)

(define dense-poly-a (make-polynomial-dense 'x '(1 2 0 3 -2 -5)))
(define dense-poly-b (make-polynomial-dense 'x '(5 0 4 -1 35)))

(add dense-poly-a dense-poly-b)
(sub dense-poly-b dense-poly-a)
(mul dense-poly-a dense-poly-b)

(add poly-a dense-poly-b)
(add dense-poly-b poly-a)
(sub poly-b dense-poly-a)
(add dense-poly-a poly-b)
(add poly-b dense-poly-a)
(sub dense-poly-b poly-a)
