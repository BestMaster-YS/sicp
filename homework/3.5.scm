(load "util.scm")

;; Monte Carlo Integration

(define (estimate-integral predicate x1 x2 y1 y2 trials)
  ;compute the area of the rectangle
  (define (lies-in-region)
    (let ((x-range (random-in-range x1 x2))
          (y-range (random-in-range y1 y2)))
      (predicate x-range y-range)))
  (* 4.0 (monte-carlo trials lies-in-region) 1.0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (unit-circle x y)
  (< (+ (square x)
        (square y))
     1))

(estimate-integral unit-circle -1.0 1.0 -1.0 1.0 100000)


