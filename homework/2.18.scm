;; reverse

(define (reverse items)
  (let ((cur (car items)
        (next (cdr items))))
    (if (null? next)
        cur
        (cons (reverse next) cur)))
)

(define test (list 1 2 3 4 5 6))

(reverse test)
