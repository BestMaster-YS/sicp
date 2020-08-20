(load "util.scm")

(define (partial-sum stream)
  (define (iter s1 s2)
    (cons-stream (stream-car s1) (iter (add-streams (stream-cdr s1) s2) s2)))
  (iter stream stream))

(stream-ref (partial-sum integers) 4)

(define (partial-sum s)
  (cons-stream (stream-car s) (add-streams (stream-cdr s) (partial-sum s))))

(stream-ref (partial-sum integers) 3)

