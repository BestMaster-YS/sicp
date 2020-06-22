(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs1 x)
  (cond ((< x 0) (- x))
        (else x)))

(and (> 10 5) (< 5 10))

; (define (>= x y)
;   (or (> x y) (= x y)))

(define (>= x y)
  (not (< x y)))

(>= 2 1)

