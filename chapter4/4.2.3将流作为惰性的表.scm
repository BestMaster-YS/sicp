;; 将car,cdr 使用函数实现（放在 lazy-eval 中），就成为了惰性的，所有的 map, filter ,for-each 等都是惰性的
(define (cons x y)
  (lambda (m) (m x y)))

(define (car p) (p (lambda (x y) x)))
(define (cdr p) (p (lambda (x y) y)))



