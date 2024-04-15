(load "vectors.scm")
(load "primitives.scm")
(load "operators.scm")

;;; Example from the Displacement Method paper [p. 283]
;;; Note the discrepancy from Eq. (2.5),
;;; probably due to 1-isosurface notation and/or typo.
(let ((p1 (cylinder '(0 0 0) '(0 0 1) 2))
      (p2 (sphere '(0 0 0) 5)))
  (show (elliptic-blend (list p1 p2) '(1 1) 3)
        '((-6 -6 0) (6 6 8)) 40))
