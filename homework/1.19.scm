(load "util.scm")
(define (fib n)
	(fib-iter2 1 0 0 1 n)
)

(define (fib-iter a b p q count)
	(cond ((= count 0) b)
        ((even? count)
         (fib-iter (+ (* b (+ (* 2 p q) (square q)))
                      (* a (+ (square q) (square p)))
                      (* a (+ (square q) (* 2 p q))))
                   (+ (* b (+ (square q) (square b)))
                      (* a (+ (square q) (* 2 p q))))
                   p
                   q
                   (halve count)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))
  )
)

(define (fib-iter2 a b p q count)
	(cond ((= count 0) b)
        ((even? count)
         (fib-iter2 a
                    b
                    (+ (square p) (square q))
                    (+ (* 2 p q) (square q))
                    (halve count)))
        (else (fib-iter2 (+ (* b q) (* a q) (* a p))
                         (+ (* b p) (* a q))
                         p
                         q
                         (- count 1)))
  )
)

(fib 100)
;; 354224848179261915075