(load "2.8.scm")

(define (width interval)
  (/ (+ (lower-bound interval) (upper-bound interval)) 2)
)
(define a (make-interval 1.0 3))
(define b (make-interval 1.0 10))

(width (add-interval a b))
(+ (width a) (width b))
(width (sub-interval a b))
(- (width a) (width b))
(width (mul-interval a b))
(* (width a) (width b))
(width (div-interval a b))
(/ (width a) (width b))

