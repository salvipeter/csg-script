(define (v+ . args) (apply map + args))
(define (v- . args) (apply map - args))
(define (v* u . args) (map (lambda (x) (apply * x args)) u))
(define (vlength u) (sqrt (apply + (map (lambda (x) (* x x)) u))))
(define (vnormalize u) (v* u (/ (vlength u))))
(define (point-distance p q) (vlength (v- q p)))
(define (scalar-product u v) (apply + (map * u v)))
(define (cross-product u v)
  (list (- (* (cadr u)  (caddr v)) (* (caddr u)  (cadr v)))
        (- (* (caddr u)   (car v)) (* (car u)   (caddr v)))
        (- (* (car u)    (cadr v)) (* (cadr u)   (car v)))))
