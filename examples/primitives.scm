(define (sphere center radius)
  (lambda (p)
    (- (point-distance p center) radius)))

(define (plane point normal)
  (lambda (p)
    (scalar-product (v- p point) normal)))

(define (cylinder point direction radius)
  (lambda (p)
    (let ((d (v- p point)))
      (- (vlength (v- d (v* direction (scalar-product d direction))))
         radius))))

;;; Note that this is not Euclidean distance
(define (cone apex direction aperture)     ; aperture in degrees, i.e. in (0,90)
  (lambda (p)
    (if (= (point-distance p apex) 0)
        0
        (let* ((u (vnormalize (v- p apex)))
               (alpha (acos (abs (scalar-product u direction)))))
          (- (/ (* 180 alpha) (angle -1)) aperture)))))

(define (torus center normal radius-large radius-small)
  (lambda (p)
    (let* ((q (v+ p (v* normal (scalar-product (v- center p) normal))))
           (dir (if (= (point-distance center q) 0) '(1 0 0) (vnormalize (v- q center))))
           (cylp (v+ center (v* dir radius-large)))
           (cyl (cylinder cylp (cross-product dir normal) radius-small)))
      (cyl p))))

(define (ellipsoid focus1 focus2 length)
  (lambda (p)
    (- (+ (point-distance p focus1)
          (point-distance p focus2))
       length)))
