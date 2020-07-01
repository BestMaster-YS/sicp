(define (square x)
  (* x x))

(define (square-of-sum x y)
  (+ (square x) (square y)))

(define (f a)
  (square-of-sum (+ a 1) (* a 2)))






