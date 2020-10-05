((lambda (n)
   ((lambda (fact) (fact fact n))
    (lambda (ft k) (if (= k 1)
                       1
                       (* k (ft ft (- k 1)))))))
 10)

((lambda (n)
   ((lambda (fib) (fib fib n))
    (lambda (fib k) (cond ((= k 0) 0)
                          ((= k 1) 1)
                          (else
                           (+ (fib fib (- k 1)) (fib fib (- k 2))))))))
 10)

(define (f x)
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? od? ev? (- n 1))))
   (lambda (od? ev? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

(f 2)
