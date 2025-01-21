(load "vectors.scm")
(load "primitives.scm")
(load "operators.scm")

(define use-marching-cubes #t)

(let* ((cyl1 (cylinder '(0 7 0) (vnormalize '(2 1 0)) 5))
       (cyl2 (cylinder '(10 0 0) '(0 1 0) 8))
       (blend (elliptic-blend (list cyl1 cyl2) '(1 1) 3)))
  (if use-marching-cubes
      (mc blend '(0 0 0) 20 '(4 6))
      (show blend '((-20 -20 -10) (30 30 10)) 20)))
