(load "vectors.scm")                    ; basic vector operations

;;; Options

(define numeric-gradient? #t)           ; much slower when turned on

(define epsilon 1e-7)                   ; used in numeric gradient computation

;;; Utilities

;;; Safely descale the vector: return (0 0 0) when dividing by 0
(define (v/ u x)
  (if (< (abs x) 1e-8)
      '(0 0 0)
      (v* u (/ x))))

(define (with-gradient f)
  (if numeric-gradient?
      (let ((deltas (map (lambda (u)
                           (v* u epsilon))
                         '((1 0 0) (0 1 0) (0 0 1)))))
        (lambda (p)
          (let ((fp (f p)))
            (cons fp
                  (map (lambda (dp)
                         (/ (- (f (v+ p dp)) fp) epsilon))
                       deltas)))))
      (lambda (p)
        (cons (f p) '(0 0 0)))))

;;; Primitives returning (value . gradient)

(define (halfspace point normal)
  (let ((n (vnormalize normal)))
    (lambda (p)
      (cons (scalar-product (v- p point) n) n))))

(define (sphere center radius)
  (lambda (p)
    (let* ((d (v- p center))
           (r (vlength d)))
      (cons (- r radius) (v/ d r)))))

(define (cylinder point direction radius)
  (let ((dir (vnormalize direction)))
    (lambda (p)
      (let* ((u (v- p point))
             (d (v- u (v* dir (scalar-product u dir))))
             (r (vlength d)))
        (cons (- r radius) (v/ d r))))))

;;; Combinators

;;; The Rockwood-Owen approximative rolling ball blend
(define (blend f1 f2 r)
  (with-gradient
   (lambda (p)
     (let* ((fx1 (f1 p))
            (fx2 (f2 p))
            (P1 (car fx1))
            (P2 (car fx2))
            (cos-theta (scalar-product (v/ (cdr fx1) (vlength (cdr fx1)))
                                       (v/ (cdr fx2) (vlength (cdr fx2)))))
            (sin-theta (sqrt (- 1 (* cos-theta cos-theta))))
            (d1 (- r (* (- r P2) cos-theta)))
            (d2 (- r (* (- r P1) cos-theta))))
       (if (and (< P1 d1) (< P2 d2))    ; on R
           (if (< (abs sin-theta) 1e-8)
               0                        ; kutykurutty
               (- r (/ (sqrt (+ (* (- r P1) (- r P1))
                                (* (- r P2) (- r P2))
                                (* -2 (- r P1) (- r P2) cos-theta)))
                       sin-theta)))
           (min P1 P2))))))

(define (se-blend f1 f2 r)
  (with-gradient
   (lambda (p)
     (let* ((fx1 (f1 p))
            (fx2 (f2 p))
            (P1 (car fx1))
            (P2 (car fx2))
            (cos-theta (scalar-product (v/ (cdr fx1) (vlength (cdr fx1)))
                                       (v/ (cdr fx2) (vlength (cdr fx2)))))
            (range (* r (- 1 cos-theta))))
       (- 1 (expt (max 0 (- 1 (/ P1 range))) 2)
          (expt (max 0 (- 1 (/ P2 range))) 2))))))

(define (complement f)
  (lambda (p)
    (let ((fx (f p)))
      (cons (- (car fx))
            (v* (cdr fx) -1)))))

(define (union . fs)
  (lambda (p)
    (let ((fx ((car fs) p)))
      (if (null? (cdr fs))
          fx
          (let ((best ((apply union (cdr fs)) p)))
            (if (< (car fx) (car best))
                fx
                best))))))

(define (intersect . fs)
  (lambda (p)
    (let ((fx ((car fs) p)))
      (if (null? (cdr fs))
          fx
          (let ((best ((apply union (cdr fs)) p)))
            (if (> (car fx) (car best))
                fx
                best))))))

(define (value f)
  (lambda (p)
    (car (f p))))

;;; Test example

(define h1 (halfspace '(0 0 0) '(1 0 0)))
(define h2 (halfspace '(0 0 0) '(2 3 0)))
(define h3 (halfspace '(0 0 0) '(0 1 0)))
(define s1 (sphere '(0 0 0) 0.4))
(define c1 (cylinder '(0 0 0) '(1 0 0.5) 0.5))
(define b1 (intersect h3 (blend h1 c1 0.5)))
(define b2 (intersect (complement h3) (se-blend h1 c1 0.5)))
(define surface (union b1 b2))

;;; Meshing

(mc (value surface) '(0 0 0) 2 '(5 7))
