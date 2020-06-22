(load "util.scm")

(define (fix-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance)
  )

  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display next)
      (if (close-enough? guess next)
          next
          (try next)))
  )
  (newline)
  (display first-guess)
  (try first-guess)
)

(fix-point (lambda (x) (/ (log 1000) (log x)))
           2)
(fix-point (lambda (x) (average (log x) (/ (log 1000) (log x))))
           2)
