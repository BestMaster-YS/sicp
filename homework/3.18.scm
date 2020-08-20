;; determines whether it contains a cycle

(define (check-cycle x)
  (let ((memo '()))
    (define (inner seq)
      (cond ((or (null? seq) (not (pair? seq))) #f)
            ((memq seq memo) #t)
            (else
              (begin (set! memo (cons seq memo))
                     (inner (cdr seq))))))
      (inner x)))

(define (make-cycle a)
  (set-cdr! (last-pair a) a)
  a)

(define a (make-cycle (list 1 2 3)))

(check-cycle a)
