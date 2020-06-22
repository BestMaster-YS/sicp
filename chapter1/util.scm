(define (cube x) (* x x x))

(define (square x)
  (* x x))

(define (even? n)
  (= (remainder n 2) 0)
)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b)))
)

(define (average a b)
  (/ (+ a b) 2.0)
)

(define tolerance 0.00001)

(define (fix-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance)
  )

  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next)))
  )
  (try first-guess)
)