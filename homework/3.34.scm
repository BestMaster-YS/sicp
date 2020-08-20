(load "util.scm")

(define (squarer a b)
  (multiplier a a b))

;; 当知道 b 的值的时候，两个 a 的值为空，并不能根据 b 和其中一个 a 算出另一个 a 的值。
;; 即 a -> b 可以，b -> a 不可以



