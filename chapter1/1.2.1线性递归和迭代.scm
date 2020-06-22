(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1))))
)

(factorial 6)

;; 用迭代的思想重构 factorial

(define (factorial n)
  (fact-iter 1 1 n)
)

(define (fact-iter product counter max-counter)
  (if (> counter max-counter)
      product
      (fact-iter (* product counter)
                 (+ counter 1)
                 max-counter))
)

(factorial 6)
