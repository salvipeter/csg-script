(use-modules (srfi srfi-1))

(define (union . surfaces)
  (lambda (p)
    (apply min (map (lambda (f) (f p)) surfaces))))

(define (intersection . surfaces)
  (lambda (p)
    (apply max (map (lambda (f) (f p)) surfaces))))

(define (complement f)
  (lambda (p)
    (- (f p))))

(define (subtract f . surfaces)
  (apply intersection f (map complement surfaces)))

(define (elliptic-blend surfaces ranges t)
  (lambda (p)
    (- 1
       (fold + 0
             (map (lambda (f r)
                    (expt (max 0 (- 1 (/ (f p) r))) t))
                  surfaces ranges)))))

(define (translate f u)
  (lambda (p)
    (f (v- p u))))

(define (scale f x)
  (lambda (p)
    (f (v* p (/ x)))))

(define (rotate f u theta)
  (lambda (p)
    (let* ((u (vnormalize u))
           (c (cos theta))
           (s (sin theta))
           (ux (list-ref u 0))
           (uy (list-ref u 1))
           (uz (list-ref u 2))
           (x (list-ref p 0))
           (y (list-ref p 1))
           (z (list-ref p 2)))
      (f (list (+ (* (+ (* ux ux (- 1 c)) c) x)
                  (* (+ (* ux uy (- 1 c)) (- (* uz s))) y)
                  (* (+ (* ux uz (- 1 c)) (* uy s)) z))
               (+ (* (+ (* ux uy (- 1 c)) (* uz s)) x)
                  (* (+ (* uy uy (- 1 c)) c) y)
                  (* (+ (* uy uz (- 1 c)) (- (* ux s))) z))
               (+ (* (+ (* ux uz (- 1 c)) (- (* uy s))) x)
                  (* (+ (* uy uz (- 1 c)) (* ux s)) y)
                  (* (+ (* uz uz (- 1 c)) c) z)))))))
