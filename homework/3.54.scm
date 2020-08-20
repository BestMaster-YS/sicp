(load "util.scm")

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

;; 构造一个递增的无穷流

(define factorials (cons-stream 1 (mul-streams factorials integers)))

;; 1 1 2 6 34 120
;; 0 1 2 3 4  5

