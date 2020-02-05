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
