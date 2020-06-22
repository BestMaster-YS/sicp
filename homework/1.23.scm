(load "1.22.scm")
;; 1.检查是否为素数 prime?
(define (next n)
  (if (= n 2)
      3
      (+ n 2))
)

(define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor))))
)

(search-for-time 1000)
(search-for-time 10000)
(search-for-time 100000)
(search-for-time 1000000)