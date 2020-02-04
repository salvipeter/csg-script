(load "vectors.scm")
(load "primitives.scm")
(load "operators.scm")

(let* ((bbox '((-2 -2 -2) (2 2 2)))
       (res 50)
       (res (list res res res))
       (pl (plane '(1/3 0 0) '(1 0 0))))
  (show (union (sphere '(0 0 0) 1) pl) bbox res))
