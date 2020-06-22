(define (gcd a b)
	(if (= b 0)
      a
      (gcd b (remainder a b)))
)

(define (fib n)
  (fib-iter 1 0 n)
)

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1)))
  )


(define (smallest-divisor n)
	  (find-divisor n 2)
)

(define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1))))  
)

(define (divides? a b)
	(= 0 (remainder b a))
)

(define (prime? n)
	(= (smallest-divisor n) n)
)
