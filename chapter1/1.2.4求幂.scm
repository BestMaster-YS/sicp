(load "util.scm")

(define (expt b n)
	(if (= n 0)
      1
      (* b (expt b (- n 1)))) 
)

(expt 3 2)

(define (expt b n)
  (expt-iter b n 1)
)

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b (- counter 1)
                   (* product b)))
)

(define (even? n)
  (= (remainder n 2) 0)
)

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1)))))
)

(fast-expt 2 25)

(define (fast-expt b n)
	(fast-expt-iter b n 1)  
)
;; 迭代方法计算幂
(define (fast-expt-iter b n a)
	(cond ((= n 0) a)
        ((even? n) (fast-expt-iter (square b)
                                   (/ n 2)
                                   a))
        (else (fast-expt-iter b (- n 1) (* a b))))
)

(fast-expt 2 100)