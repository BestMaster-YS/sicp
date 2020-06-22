(load "util.scm")
(define (filter-accumulate filter combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((filter a) (combiner (term a) (filter-accumulate filter
                                                          combiner
                                                          null-value
                                                          term
                                                          (next a)
                                                          next
                                                          b)))
        (else (filter-accumulate filter combiner null-value term (next a) next b)))
)

(define (sum-prime a b)
  (filter-accumulate prime? + 0 identity a inc b)
)

(sum-prime 2 10)

;;小于 n 的所有与 n 互素的正整数

(define (sum-prime-each n)
  (define (prime-each? i)
    (= (gcd i n) 1)
  )
  (filter-accumulate prime-each? + 0 identity 1 inc n)
)

(sum-prime-each 10)

