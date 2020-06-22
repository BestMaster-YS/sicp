(load "util.scm")

(fix-point (lambda (x) (+ 1 (/ 1 x))) 1.0)