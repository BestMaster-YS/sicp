(load "util.scm")

;; repeat f 1 = f
;; repeat f 2 = compose f f
;; repeat f 3 = compose (compose f f) f
;; repeat f n = compose (repeat f n-1) f

(define (repeat f n)
  (if (= n 1)
      (lambda (x) (f x))
      (compose (repeat f (- n 1)) f))
)

((repeat square 3) 5)