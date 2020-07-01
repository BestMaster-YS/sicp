(define (make-accmulate init)
  (lambda (addend)
    (begin (set! init (+ init addend))
           init)))

(define A (make-accmulate 5))

(A 10)
(A 10)
