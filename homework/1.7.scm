(load "util.scm")

;; 重写 good-enough

(define (good-enough? guess x)
  (< (/ (abs (- guess (improve guess x))) guess)
     0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 10000000000000000000000000000000000000000)

(sqrt 0.00009)