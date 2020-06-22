(load "util.scm")

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