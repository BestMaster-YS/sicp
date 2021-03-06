(load "util.scm")

(define (integral delay-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delay-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     ;; 在这里需要再次添加 delay
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (y) y) 1 0.001) 1000)

