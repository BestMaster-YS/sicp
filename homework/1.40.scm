(load "util.scm")

(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c))
)

(define (cubic-root a b c)
  (newton-method (cubic a b c) 1.0)
)

(cubic-root 1 1 1)