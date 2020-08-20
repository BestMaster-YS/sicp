(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)


(define z (make-cycle (list 'a 'b 'c)))

;; 陷入无限循环
(last-pair z)

