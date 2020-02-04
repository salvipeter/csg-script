(define (union f g)
  (lambda (p)
    (min (f p) (g p))))

(define (intersection f g)
  (lambda (p)
    (max (f p) (g p))))

(define (complement f)
  (lambda (p)
    (- (f p))))

(define (subtract f g)
  (intersection f (complement g)))

(define (elliptic-blend f f-range g g-range t)
  (lambda (p)
    (- 1
       (expt (max 0 (- 1 (/ (f p) f-range))) t)
       (expt (max 0 (- 1 (/ (g p) g-range))) t))))
