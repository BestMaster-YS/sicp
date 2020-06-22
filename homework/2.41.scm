(load "util.scm")

(define (sum-list seq)
  (accmulate + 0 seq))

(define (removes s t)
  (accmulate (lambda (v result)
	       (remove v result))
	     s
	     t))

;; 生成三元组
(define (make-unique-triad n)
  (flatmap (lambda (k)
	     (map reverse
		  (map (lambda (pair) (cons k pair))
		       (unique-pair (- k 1)))))
	   (enumerate-interval 3 n)))

(define (sum-triad n s)
  (filter (lambda (seq) (= s (sum-list seq)))
	  (make-unique-triad n)))

(sum-triad 10 15)




