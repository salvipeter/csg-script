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
