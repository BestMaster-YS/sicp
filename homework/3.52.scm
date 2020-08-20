(load "util.scm")

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))

(display sum)

(define y (stream-filter even? seq))

(display sum)

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(display sum)

(stream-ref y 7)

(display-stream z)
