(load "util.scm")

(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1)) (next1 (cdr set1))
                (x2 (car set2)) (next2 (cdr set2)))
            (cond ((= x1 x2)
                   (cons x1 (union-set next1 next2)))
                  ((> x1 x2)
                   (cons x2 (union-set set1 next2)))
                  ((< x1 x2)
                   (cons x1 (union-set next1 set2))))))))


(union-set '(1 2 3 5) '(1 3 5 7))
