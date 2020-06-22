(load "util.scm")

(define (d i)
  (if (= (remainder (+ i 1) 3) 0)
      (* 2 (/ (+ i 1) 3))
      1)
)

(define (e k)
  (+ 2.0
     (cont-frac (lambda (i) 1)
                d
                k))
)

(e 100)