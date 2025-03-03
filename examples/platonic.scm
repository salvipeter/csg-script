(load "vectors.scm")
(load "primitives.scm")
(load "operators.scm")

(define phi (/ (+ 1 (sqrt 5)) 2))

(define (platonic-solid normals r)
  (let ((face (lambda (x)
                (let* ((conv (lambda (x)
                               (case x
                                 ((f) phi)
                                 ((m) (- phi))
                                 ((f1) (/ phi))
                                 ((m1) (- (/ phi)))
                                 (else x))))
                       (n (vnormalize (map conv x))))
                  (halfspace (v* n r) n)))))
    (apply intersection (map face normals))))

(define octahedron
  (platonic-solid '((1 1 1) (1 1 -1) (1 -1 1) (1 -1 -1)
                    (-1 1 1) (-1 1 -1) (-1 -1 1) (-1 -1 -1))
                  (/ (sqrt 3))))

(define dodecahedron
  (platonic-solid '((0 1 f) (0 1 m) (0 -1 f) (0 -1 m)
                    (1 f 0) (1 m 0) (-1 f 0) (-1 m 0)
                    (f 0 1) (f 0 -1) (m 0 1) (m 0 -1))
                  (/ phi (sqrt (- 9 (* 3 phi))))))

(define icosahedron
  (platonic-solid '((1 1 1) (1 1 -1) (1 -1 1) (1 -1 -1)
                    (-1 1 1) (-1 1 -1) (-1 -1 1) (-1 -1 -1)
                    (0 f f1) (0 f m1) (0 m f1) (0 m m1)
                    (f f1 0) (f m1 0) (m f1 0) (m m1 0)
                    (f1 0 f) (f1 0 m) (m1 0 f) (m1 0 m))
                  (/ (+ phi 1) (sqrt (* 3 (+ phi 2))))))

(define (hyperbolize f alpha)
  (lambda (p)
    (f (v* p (expt (vlength p) (- alpha))))))

;; (mc (hyperbolize octahedron 1/3)
;;     '(0 0 0) 1.5 '(4 6))

;; (mc (hyperbolize dodecahedron 2/3)
;;     '(0 0 0) 1.5 '(4 6))

(mc (hyperbolize icosahedron 2/3)
    '(0 0 0) 1.5 '(4 6))
