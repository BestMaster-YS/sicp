(load "util.scm")

(define (smooth f)
  (lambda (x) (/ (+ (f x) (f (+ x dx)) (f (- x dx)))
                 3))
)

;; smooth

(define (smooth-repeat f n)
  (lambda (x) (((repeat smooth n) f) x))
)

((smooth-repeat square 5) 5)