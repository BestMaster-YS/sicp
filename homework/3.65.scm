(load "util.scm")


(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2 (partial-sum (ln2-summands 1)))

(take 10 ln2)
(take 10 (euler-transform ln2))
(take 10 (accelerated-squence euler-transform ln2))

