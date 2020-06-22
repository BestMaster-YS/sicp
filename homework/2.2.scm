(define (make-point x y) (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (make-segment p1 p2) (cons p1 p2))

(define (start-segment s) (car s))

(define (end-segment s) (cdr s))

(define (midpoint-segment segment)
  (make-point (/ (+ (x-point (start-segment segment))
                    (x-point (end-segment segment)))
                  2)
              (/ (+ (y-point (start-segment segment))
                    (y-point (end-segment segment)))
                  2))
)

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
)

(define x1 (make-point 1 2))
(define x2 (make-point 3 6))

(print-point (midpoint-segment (make-segment x1 x2)))


