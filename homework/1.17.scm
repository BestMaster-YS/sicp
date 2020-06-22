(load "util.scm")

;; 用加法求乘积
(define (* a b)
	(if (= b 0)
      0
      (+ a (* a (- b 1))))	  
)

(* 123 456)

(define (* a b)
	(product-iter a b 0)
)

(define (product-iter a b c)
	(cond ((= b 0) c)
        ((= a 0) c)
        ((even? b) (product-iter (double a)
                                 (halve b)
                                 c))
        (else (product-iter a (- b 1) (+ a c))))
)

(* 123 456)