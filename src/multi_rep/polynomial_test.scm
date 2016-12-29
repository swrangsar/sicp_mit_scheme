(load "multi_rep/table_2d")
(load "multi_rep/tagged_data")
(load "multi_rep/data_directed")
(load "multi_rep/arithmetic")
(load "multi_rep/symbolic_algebra")


(install-scheme-number-package)
(install-polynomial-package)


(define poly-a (make-polynomial 'x '((5 1) (4 2) (2 3) (1 -2) (0 -5))))
(define poly-b (make-polynomial 'x '((100 1) (2 2) (0 1))))

(add poly-a poly-b)
(sub poly-b poly-a)
(mul poly-a poly-b)
