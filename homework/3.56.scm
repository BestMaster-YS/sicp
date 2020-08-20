(load "util.scm")

;; merge 将排好序的s1,s2按顺序排列，并去重
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((scale1 (stream-car s1))
               (scale2 (stream-car s2)))
           (cond ((< scale1 scale2)
                  (cons-stream scale1 (merge (stream-cdr s1) s2)))
                 ((< scale2 scale1)
                  (cons-stream scale2 (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream scale1 (merge (stream-cdr s1) (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream 2 S) (merge (scale-stream 3 S) (scale-stream 5 S)))))







