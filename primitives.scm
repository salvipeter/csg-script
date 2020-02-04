(define (sphere center radius)
  (lambda (p)
    (- (vlength (v- p center)) radius)))

(define (plane point normal)
  (lambda (p)
    (scalar-product (v- p point) normal)))

(define (cylinder point direction radius)
  (lambda (p)
    (let ((d (v- p point)))
      (- (vlength (v- d (v* direction (scalar-product d direction))))
         radius))))
