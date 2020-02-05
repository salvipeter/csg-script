(load "vectors.scm")
(load "primitives.scm")
(load "operators.scm")

(let* ((cyl1 (cylinder '(0 0 0) '(1 0 0) 5))
       (cyl2 (cylinder '(0 0 0) '(0 1 0) 5))
       (cyl3 (cylinder '(0 0 0) '(0 0 1) 5))
       (pl1 (plane '(0 0 0) '(-1 0 0)))
       (pl2 (plane '(0 0 0) '(0 -1 0)))
       (pl3 (plane '(0 0 0) '(0 0 -1)))
       (blend (elliptic-blend (list (intersection cyl1 pl1)
                                    (intersection cyl2 pl2)
                                    (intersection cyl3 pl3))
                              '(1 1 1) 3))
       (pl1 (plane '(10 0 0) '(1 0 0)))
       (pl2 (plane '(0 10 0) '(0 1 0)))
       (pl3 (plane '(0 0 10) '(0 0 1)))
       (sp (sphere '(0 0 0) 5))
       (sp1 (sphere '(10 0 0) 5))
       (sp2 (sphere '(0 10 0) 5))
       (sp3 (sphere '(0 0 10) 5))
       (trebol (union (intersection blend pl1 pl2 pl3) sp sp1 sp2 sp3)))
  (show trebol '((-20 -20 -20) (20 20 20)) 40))