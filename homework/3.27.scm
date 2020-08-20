(load "util.scm")

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((prev-computed-result (lookup x table)))
        (or prev-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize
   (lambda (n)
     (cond ((= n 0) 0)
           ((= n 1) 1)
           (else
            (+ (memo-fib (- n 1))
               (memo-fib (- n 2))))))))

(memo-fib 19999)



