(load "util.scm")

(define (stream-limit s tolerance)
  (let ((cur (stream-car s))
        (next (stream-car (stream-cdr s))))
    (if (< (abs (- next cur)) tolerance)
        next
        (stream-limit (stream-cdr s) tolerance))))

(define (sqrt s tolerance)
  (stream-limit (sqrt-stream s) tolerance))

(sqrt 2 0.00001)

