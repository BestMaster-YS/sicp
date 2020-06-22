(load "1.22.scm") 

(define (continue-primes n count)
  (cond ((= count 0)
         (newline)
         (display "all primes."))
        ((fast-prime? n 10)
         (newline)
         (display n)
         (continue-primes (next-odd n) (- count 1)))
        (else
         (continue-primes (next-odd n) count)))
)
