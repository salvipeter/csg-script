(load "vectors.scm")
(load "primitives.scm")
(load "operators.scm")

(let* ((t1 (torus '(0 0 0) '(1 0 0) 14 3))
       (t2 (torus '(0 0 0) '(0 1 0) 14 3))
       (t3 (torus '(0 0 0) '(0 0 1) 14 3)))
  (show (union t1 t2 t3) '((-20 -20 -20) (20 20 20)) 50))
