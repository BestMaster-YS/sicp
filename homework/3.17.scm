
(define (count-pair x)
  (define (inner seq memo)
    (if (and (pair? seq)
             (not (null? seq))
             (not (memq seq memo)))
        (begin (set! memo (append memo seq))
               (+ (inner (car seq) memo)
                  (inner (cdr seq) memo)
                  1))
        0))
  (inner x '()))

(define one (list 1))
(define two (cons one one))

(define three (list 1 2 3))

(define four (list two (cdr two)))
(define five (list two two))

(count-pair three)
(count-pair four)
(count-pair five)
