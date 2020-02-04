(define (sphere center radius)
  (lambda (p)
    (- (vlength (v- p center)) radius)))

(define (plane point normal)
  (lambda (p)
    (scalar-product (v- p point) normal)))
