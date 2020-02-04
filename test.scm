(load "vectors.scm")
(load "primitives.scm")
(load "operators.scm")

(let ((bbox '((-2 -2 -2) (2 2 2)))
      (res '(50 50 50))
      (pl (plane '(0 0 0) '(1 0 0))))
  (show (subtract (sphere '(0 0 0) 1) pl) bbox res)
  (show pl bbox res))

(error "Baj van")
