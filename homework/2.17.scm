(define (last-pair items)
  (let ((cur (car items))
        (next (cdr items)))
    (if (null? next)
        cur
        (last-pair next)))
)

(define list1 (list 1 2 3 4 5))

(last-pair list1)