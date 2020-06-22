(load "2.9.scm")

(define (div-interval a b)
  (if (<= (* (lower-bound b) (upper-bound b)) 0)
      (error "Division error (interval spans 0)" b)
      (mul-interval a (make-interval (/ 1.0 (lower-bound b))
                                     (/ 1.0 (upper-bound b)))))
)