(load "util.scm")

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (unit-circle x y)
  (< (+ (square x)
        (square y))
     1))

(define (lies-in-region predicate x1 x2 y1 y2)
  (let ((x-range (random-in-range x1 x2))
        (y-range (random-in-range y1 y2)))
    (if (predicate x-range y-range)
        1
        0)))

(define (generate-proc-stream proc)
  (cons-stream (proc)
               (generate-proc-stream proc)))

(define (divides-stream s1 s2)
  (let ((s1-car (stream-car s1))
        (s2-car (stream-car s2)))
    (cons-stream
     (/ s1-car s2-car)
     (divides-stream (stream-cdr s1) (stream-cdr s2)))))


(define (estimate-integral-stream predicate x1 x2 y1 y2)
  (define res-stream
    (cons-stream
     (lies-in-region predicate x1 x2 y1 y2)
     (add-streams (generate-proc-stream (lambda () (lies-in-region predicate x1 x2 y1 y2)))
                  res-stream)))
  (scale-stream  (divides-stream
                  res-stream
                  (stream-cdr integers))
                 4.0))

(stream-ref (estimate-integral-stream unit-circle -1.0 1.0 -1.0 1.0) 100000)


