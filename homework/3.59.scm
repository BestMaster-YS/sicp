(load "util.scm")

(define (integrate-series stream)
  (stream-map *
              stream
              (stream-map / ones integers)))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define sine-stream
  (cons-stream 0 (integrate-series cosine-series)))

(define cosine-series
  (cons-stream 1 (integrate-series (scale-stream sine-stream -1))))
