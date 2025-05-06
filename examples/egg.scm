(load "vectors.scm")
(load "primitives.scm")
(load "operators.scm")

(define s (sphere '(0 0 0) 3))
(define e (focoid '((-4 0 0) (4 0 0)) 10))
(define p (halfspace '(0 0 0) '(1 0 0)))
(define surface (union s (intersection p e)))

(mc surface '(0 0 0) 6 '(4 8))
