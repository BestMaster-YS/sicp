(define (fringe tree)
  (define cur (if (pair? (car tree))
                  (fringe (car tree))
                  (list (car tree))))
  (define next (cdr tree))
  (if (null? next)
      cur
      (append cur (fringe next)))
)

(define x (list (list 1 2) (list 3 4)))

(fringe x)
(fringe (list x x))
