(load "2.12.scm")

(define a (make-center-percent 10 0.1))
(define b (make-center-percent 15 0.1))

(center (div-interval a a))
(percent (div-interval a a))

(center (div-interval a b))
(percent (div-interval a b))
