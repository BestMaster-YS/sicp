(load "util.scm")

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b))
)

(define (car z)
  (if (= (gcd z 2) 1)
      0
      (+ 1 (car (/ z 2))))
)

(define (cdr z)
  (if (= (gcd z 3) 1)
      0
      (+ 1 (cdr (/ z 3))))
)

(define test (cons 4 6))
(car test)
(cdr test)

