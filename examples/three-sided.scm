(load "vectors.scm")
(load "primitives.scm")
(load "operators.scm")

(define q3/2 (/ (sqrt 3) 2))
(define res 50)

(let* ((size 0.9)
       (r size)
       (bottom (/ size -2 (sqrt 3)))
       (s1 (sphere (list (/ size 2) bottom 0) r))
       (s2 (sphere (list (/ size -2) bottom 0) r))
       (s3 (sphere (list 0 q3/2 0) r)))
  ;(show (intersection s1 s2 s3) '((-1 -1 -1) (1 1 1)) res)
  (mc (intersection s1 s2 s3) '(0 0 0) 1.4 '(5 7))
  )

(let* ((size 0.8)
       (bottom (/ size -2 (sqrt 3)))
       (p1 (halfspace (list (/ size 2) bottom 0) (list q3/2 0.5 0)))
       (p2 (halfspace (list (/ size -2) bottom 0) (list (- q3/2) 0.5 0)))
       (p3 (halfspace (list 0 bottom 0) (list 0 -1 0)))
       (s (sphere '(0 0 1.5) (/ size 2))))
  ;(show (intersection s p1 p2 p3) '((-1 -1 0.8) (1 1 2.2)) res)
  (mc (intersection s p1 p2 p3) '(0 0 0) 2.4 '(5 7))
  )
