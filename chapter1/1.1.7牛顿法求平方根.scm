;; 定义求平方根

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

;; 平均值
(define (average x y)
  (/ (+ x y) 2))

;; 进一步提高准确性
(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))


(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 2)

(sqrt 0.00001)

(sqrt 1000000000000000000000000000000000)

(sqrt 9)