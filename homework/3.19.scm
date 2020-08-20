(define (cycle-check seq)
  (let ((x1 seq)
        (x2 seq))
    (define (inner fast slow)
      (if (eq? fast slow)
          #t
          (inner (cdr (cdr fast)) (cdr slow))))
    (inner x1 x2)))

(define (make-cycle a)
  (set-cdr! (last-pair a) a)
  a)


(cycle-check (make-cycle (list 1 2 3)))
