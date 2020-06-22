(load "1.24.scm")

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(search-for-time 1000)
(search-for-time 10000)
(search-for-time 100000)