;; 迭代计算过程
(define (+ a b)
  (if (= a 0)
      b
      (+ (inc a) (dec b)))
)

;; 递归计算过程
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b)))
)