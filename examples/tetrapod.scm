(load "vectors.scm")
(load "primitives.scm")
(load "operators.scm")

(define (deg2rad deg)
  (* deg (/ (atan 1) 45)))

;;; Special cone:
;;; - axis contains the origin
;;; - the given radius is taken at a distance of 1 unit from the origin
(define (leg-cone radius direction aperture)
  (let ((apex (v* direction (+ 1 (/ radius (tan (deg2rad aperture)))))))
    (cone apex direction aperture)))

(define (leg v r h)
  (let* ((c1 (leg-cone r v 12))
         (c2 (leg-cone r v 45))
         (p1 (halfspace (v* v h) v))
         (p2 (halfspace '(0 0 0) (v* v -1))))
    (intersection c1 c2 p1 p2)))

(define (tetrapod r h)
  (let ((r3 (/ (sqrt 3))))
    (union (leg (v* '(-1  1 -1) r3) r h)
           (leg (v* '( 1 -1 -1) r3) r h)
           (leg (v* '( 1  1  1) r3) r h)
           (leg (v* '(-1 -1  1) r3) r h))))

(show (subtract (tetrapod 0.3 1.07) (tetrapod 0.25 1.02))
      '((-1.2 -1.2 -1.2) (1.2 1.2 1.2)) 100)
