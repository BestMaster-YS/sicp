(define (expmod base exp m)
    (cond ((= exp 0)
            1)
          ((nontrivial-square-root? base m)                 ; 新增
            0)                                              ;
          ((even? exp)
            (remainder (square (expmod base (/ exp 2) m))
                       m))
          (else
            (remainder (* base (expmod base (- exp 1) m))
                       m))))

(define (nontrivial-square-root? a n)
  (and (not (= a 1))
       (not (= a (- n 1)))
       (= 1 (remainder (square a) n))))

(define (no-zero-random n)
  (let ((r (random n)))
    (if (not (= r 0))
        r
        (no-zero-random n)))
)

(define (miller-rabin-test n)
  (define (test-iter times)
    (cond ((= times 0) #t)
          ((= (expmod (no-zero-random n) (- n 1) n) 1)
           (test-iter (- times 1)))
          (else #f))
  )
  (let ((times (ceiling (/ n 2))))
    (test-iter times))
)

(miller-rabin-test 561)
(miller-rabin-test 1105)
(miller-rabin-test 1729)
(miller-rabin-test 2465)
(miller-rabin-test 8467498111154045898989056591111)
(miller-rabin-test 17)
(miller-rabin-test 19)
(miller-rabin-test 29)