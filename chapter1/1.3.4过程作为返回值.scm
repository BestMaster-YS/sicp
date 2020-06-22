(load "util.scm")
; 平均阻尼法
(define (average-damp f)
  (lambda (x) (average x (f x)))
)

(define (sqrt x)
  (fix-point (average-damp (lambda (y) (/ x y))) 1.0)
)

(sqrt 10)

(define (cube-root x)
  (fix-point (average-damp (lambda (y) (/ x (square y)))) 1.0)
)

(cube-root 27)


;; 牛顿法
(define dx 0.00001)

(define (deriv g)
	(lambda (x) (/ (- (g (+ x dx))
                    (g x))
                 dx))
)

((deriv cube) 5)

(define (newton-transform g)
	(lambda (x) (- x
                 (/ (g x)
                    ((deriv g) x))))  
)

(define (newton-method g guess)
	(fix-point (newton-transform g) guess)  
)

(define (sqrt x)
  (newton-method (lambda (y) (- (square y) x)) 1.0)
)

(sqrt 10)

; 抽象和第一级过程

(define (fixed-point-of-transform g transform guess)
  (fix-point (transform g) guess)
)
