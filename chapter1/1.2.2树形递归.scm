(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2)))))
)

(fib 10)


(define (fib n)
  (fib-iter 1 0 n)
)

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1)))
)

(fib 100)

;; 换硬币实例
(define (coins-type n)
  (cond ((= n 1) 1)
        ((= n 2) 5)
        ((= n 3) 10)
        ((= n 4) 25)
        ((= n 5) 50))
)
(define (coins-change amount ctype)
  (cond ((= amount 0) 1)
        ((< amount 0) 0)
        ((= ctype 0) 0)
        (else (+ (coins-change amount (- ctype 1))
                 (coins-change (- amount (coins-type ctype))
                               ctype)))
  )
)
(define (coins n)
  (coins-change n 5)
)

(coins 100)
