(load "../homework/util.scm")

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (y) y) 1 0.001) 1000)

;; 规范求值序
;; 在显式和隐式的应用延迟求值会导致程序的复杂性增加
;; 一种方式是让所有的过程都用延迟参数，采纳一种求值模型，其中过程参数都是自动延迟

