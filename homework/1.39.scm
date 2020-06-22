(load "util.scm")

(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                   x
                   (- (square x))))
             (lambda (n) (- (* 2 n) 1))
             k)
)

(tan-cf 10.0 100)
(tan 10)
