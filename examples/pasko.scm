(load "vectors.scm")
(load "primitives.scm")
(load "operators.scm")

(define (fb x n)
  (if (>= x n)
      0
      (* 1/4 n (expt (- (/ x n) 1) 2))))

(let* ((p1 (halfspace '(0 0 0) '( 1 -1 0)))
       (p2 (halfspace '(0 0 0) '(-1 -1 0)))
       (a0 2.4)
       (a1 2)
       (a2 2)
       (blend (lambda (p)               ; Pasko 1995
                (let ((d1 (p1 p))
                      (d2 (p2 p)))
                  (+ d1 d2 (vlength (list d1 d2))
                     (/ a0 (+ 1 (expt (/ d1 a1) 2) (expt (/ d2 a2) 2)))))))
       (blend2 (lambda (p)              ; Dekkers 2004
                 (let ((d1 (p1 p))
                       (d2 (p2 p)))
                   (+ (max d1 d2) (fb (abs (- d1 d2)) a0)))))
       (blend3 (lambda (p)
                 (- 1
                    (fold + 0
                          (map (lambda (f r)
                                 (expt (max 0 (- 1 (/ (- (f p)) r))) a0))
                               (list p1 p2) (list a1 a2)))))))
  (show ;(lambda (p) ((intersection p1 p2) p))
        blend3
        ;(lambda (p) (+ (blend3 p) 0.3))
        '((-2 -2 -2) (2 2 2)) 40))
