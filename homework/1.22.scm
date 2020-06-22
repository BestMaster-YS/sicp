(load "util.scm")

(define (time-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime))
)

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-time (- (runtime) start-time)))
)

(define (report-time elapsed-time)
  (display "***")
  (display elapsed-time)
)

;; 1.检查是否为素数 prime?

;; 2.获取奇数
(define (next-odd n)
  (if (odd? n)
      (+ n 2)
      (+ n 1))
)

;; 3.从n开始连续的是否为素数的奇数判断
(define (continue-primes n count)
  (cond ((= count 0)
         (display "all primes."))
        ((prime? n)
         (newline)
         (display n)
         (continue-primes (next-odd n) (- count 1)))
        (else
         (continue-primes (next-odd n) count)))
)

(define (search-for-time n)
  (let ((start-time (real-time-clock)))
        (continue-primes n 3)
        (- (real-time-clock) start-time))
)