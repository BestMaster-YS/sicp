(load "2.11.scm")

(define (make-center-percent c p)
  (make-center-width c (* c p))
)

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2)
)

(define (percent i)
  (/ (width i) (center i))
)
