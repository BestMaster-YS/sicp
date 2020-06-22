(load "util.scm")

(define x (list (list 1 2) (list 3 4)))
(define y (list 1 2 3))
(define z (list 1 (list 2 3 (list (list 4 5) 6))))

(define (deep-reverse tree)
  (define cur (if (pair? (car tree))
                  (list (deep-reverse (car tree)))
                  (list (car tree))))
  (define next (cdr tree))
  (if (null? next)
      cur
      (append (deep-reverse next) cur))
)

(deep-reverse x)
(deep-reverse y)
(deep-reverse z)
