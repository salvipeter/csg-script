;;; Implicit blend tests based on [Gourmel et al. 2013]
;;; Written in csg-script (https://github.com/salvipeter/csg-script/)
;;; Ported to libfive (https://libfive.com/)

;;; Different authors use different conventions:
;;; [R] Ricci:            1-isosurface   (< 1   inside) [similarly T-isosurface in Blinn]
;;; [H] Hoffman-Hopcroft: 0-isosurface   (< 0   inside) [also Rockwood]
;;; [P] Pasko et al.:     0-isosurface   (> 0   inside)
;;; [G] Gourmel et al.:   0.5-isosurface (> 0.5 inside) "local field functions"
;;;     -> the function is assumed to be constant 0 outside its domain
;;; This system uses [H].

;;; System-specific setup

;;; csg-script
(define-syntax if-positive
  (syntax-rules ()
    ((_ x pos neg)
     (if (> x 0) pos neg))))
(define (render f tessellation)
  (case tessellation
    ((dc) (show f '((-3 -3 -3) (3 3 3)) 100))
    ((mc) (mc f '(0 0 0) 3 '(4 7)))
    ((bl) (bl f '(0 0 0) 3 60))))

;;; libfive
;; (set-bounds! [-3 -3 -3] [3 3 3])
;; (set-quality! 8)
;; (set-resolution! 10)
;; (define (if-positive x pos neg)
;;   (let ((alpha (max 0 (compare x 0))))
;;     (+ (* pos alpha) (* neg (- 1 alpha)))))
;; (define (render f tessellation)
;;   (lambda-shape (x y z)
;;     (f (list x y z))))

;;; Utilities

(define (reduce f lst)
  (if (null? (cdr lst))
      (car lst)
      (f (car lst) (reduce f (cdr lst)))))

;;; Returns the lower solution of
;;;   a x^2 + b x + c = 0
(define (solve-quadratic a b c)
  (if (= a 0)
      (if (= b 0)
          0                             ; kutykurutty
          (- (/ c b)))
      (let ((D (- (* b b) (* 4 a c))))
           (if-positive (- D)
               0                        ; kutykurutty
               (let* ((d (sqrt (max D 0)))
                      (x1 (/ (+ (- b) d) (* 2 a)))
                      (x2 (/ (- (- b) d) (* 2 a))))
                 (min x1 x2))))))

(define (localize f)
  (lambda (p)
    (max (- 0.5 (f p)) 0)))

(define (delocalize f)
  (lambda (p)
    (- 0.5 (f p))))

;;; Point/Vector handling

(define x car)
(define y cadr)
(define z caddr)
(define (v+ . args) (apply map + args))
(define (v- . args) (apply map - args))
(define (v* u . args) (map (lambda (x) (apply * x args)) u))
(define (vlength u) (sqrt (apply + (map (lambda (x) (* x x)) u))))
(define (vnormalize u) (v* u (/ (vlength u))))

;;; Primitives

(define (sphere p)
  (- (vlength p) 1))

(define (cube p)
  (- (apply max (map abs p)) 1))

(define (cylinder p)
  (- (vlength (list (x p) (y p) 0)) 1))

;;; Transformations

(define (translate f u)
  (lambda (p)
    (f (v- p u))))

(define (scale f x)
  (lambda (p)
    (f (v* p (/ x)))))

(define (complement f)
  (lambda (p)
    (- (f p))))

;;; Operators

(define (union f1 f2)                   ; Eq. (7) [P]
  (lambda (p)
    (max (f1 p) (f2 p))))

(define (clean-union f1 f2)             ; Eq. (8) [H]
  (lambda (p)
    (let ((v1 (f1 p))
          (v2 (f2 p)))
      (+ v1 v2 (- (sqrt (+ (* v1 v1) (* v2 v2))))))))

;;; t = 1 case of `blend-ricci`
(define (blend-blinn f1 f2)             ; Eq. (3) [G]
  (lambda (p)
    (+ (f1 p) (f2 p))))

;;; t > 0 -> union; t < 0 -> intersection
(define (blend-ricci f1 f2 t)           ; Eq. (11) [G]
  (lambda (p)
    (expt (+ (expt (f1 p) t)
             (expt (f2 p) t))
          (/ t))))

(define (blend-rockwood f1 f2 r1 r2 t)  ; Eq. (4) [H] - wrong, also wrong in [Rockwood 1989]
  (lambda (p)
    (- 1
       (expt (max 0 (/ (- 1 (f1 p)) r1)) t)
       (expt (max 0 (/ (- 1 (f2 p)) r2)) t))))

(define (blend-rockwood-correct f1 f2 r1 r2 t) ; [H]
  (lambda (p)
    (- 1
       (expt (max 0 (- 1 (/ (f1 p) r1))) t)
       (expt (max 0 (- 1 (/ (f2 p) r2))) t))))

(define (blend-pasko f1 f2 a0 a1 a2)    ; Eq. (5) [P] - wrong: this is an intersection blend
  (lambda (p)
    (let ((v1 (f1 p))
          (v2 (f2 p)))
      (+ v1 v2 (- (sqrt (+ (* v1 v1) (* v2 v2))))
         (/ a0 (+ 1 (expt (/ v1 a1) 2) (expt (/ v2 a2) 2)))))))

(define (blend-pasko-correct f1 f2 a0 a1 a2) ; [H] (adapted from [Pasko et al. 1995]
  (lambda (p)
    (let ((v1 (f1 p))
          (v2 (f2 p)))
      (- (+ v1 v2)
         (sqrt (+ (* v1 v1) (* v2 v2)))
         (/ a0 (+ 1 (expt (/ v1 a1) 2) (expt (/ v2 a2) 2)))))))

(define (blend-barthe f1 f2 theta)      ; Eq. (12) [G] + [Barthe et al. 2003, Eq. (14)]
  (let* ((theta1 theta)
         (theta2 (- (acos 0) theta1))
         (tt1 (tan theta1))
         (ct2 (/ (tan theta2)))
         (a (+ (* (expt (- tt1 1) 2) ct2 ct2)
               (* (expt (- ct2 1) 2) tt1 tt1)
               (- (* (expt (- ct2 1) 2)
                     (expt (- tt1 1) 2)))))
         (bx (* (expt (- tt1 1) 2) ct2))
         (by (* (expt (- ct2 1) 2) tt1))
         (cx (expt (- tt1 1) 2))
         (cy (expt (- ct2 1) 2))
         (C (lambda (X Y)
              (let ((b (* -2 (+ (* X bx) (* Y by))))
                    (c (+ (* X X cx) (* Y Y cy))))
                (solve-quadratic a b c)))))
    (lambda (p)
      (let ((v1 (f1 p))
            (v2 (f2 p)))
        (if-positive (* (max 0 (- v2 (* v1 tt1)))
                        (max 0 (- v1 (* v2 ct2))))
            (C v1 v2)
            (max v1 v2))))))

;;; Examples

(define s1 (translate sphere '(-1.7 0 0)))
(define s2 (translate sphere '(0.5 0 0)))
(define b1 (translate cube '(1 1 1)))
(define c1 (translate cylinder (vnormalize '(-1 1 0))))

(case 'boxcyl
  ((union)
   (render (complement (reduce union (map complement (list s1 s2 b1 c1)))) 'mc))
  ((clean)
   (render (reduce clean-union (list s1 s2 b1 c1)) 'mc))
  ((boxcyl)
   (case 'rockwood
     ((ricci)
      (render (delocalize (blend-ricci (localize b1) (localize c1) 3)) 'bl))
     ((rockwood)
      (render (blend-rockwood-correct b1 c1 0.3 0.3 2) 'mc))
     ((pasko)
      (render (blend-pasko-correct b1 c1 0.08 0.3 0.3) 'mc))
     ((barthe)                          ; 0.5 ~ 28.5 degrees
      (render (delocalize (blend-barthe (localize b1) (localize c1) 0.5)) 'mc))))
  ((spheres)
   (case 'ricci
     ((ricci)
      (render (delocalize (blend-ricci (localize s1) (localize s2) 3/2)) 'bl))
     ((rockwood)
      (render (blend-rockwood-correct s1 s2 0.7 0.7 2) 'bl))
     ((pasko)
      (render (blend-pasko-correct s1 s2 0.2 0.3 0.3) 'bl))
     ((barthe)                          ; 0.4 ~ 23 degrees
      (render (delocalize (blend-barthe (localize s1) (localize s2) 0.4)) 'mc))))
  ((test)
   ((delocalize (blend-barthe (localize b1) (localize c1) 0.3))
    '(0 0 0))))
