(load "util.scm")

(define (unique-pair n)
  (flatmap (lambda (x)
	     (map (lambda (y)
		    (list x y))
		  (enumerate-interval 1 (- x 1))))
           (enumerate-interval 2 n)))

(unique-pair 5)

(define (prime-sum-pair n)
  (map make-pair-sum
       (filter prime-sum? (unique-pair n))))

(prime-sum-pair 10)





