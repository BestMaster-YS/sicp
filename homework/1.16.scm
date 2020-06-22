(load "util.scm")

(define (fast-expt b n)
	(fast-expt-iter b n 1)  
)
;; 迭代方法计算幂
(define (fast-expt-iter b n a)
	(cond ((= n 0) 1)
        ((even? n) (fast-expt-iter (square b)
                                   (/ n 2)
                                   a))
        (else (fast-expt-iter b (- n 1) (* a b))))
)