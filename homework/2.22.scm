(load "util.scm")

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr thing)
              (cons (square (car things))
                    answer)))
  )
  (reverse (iter items nil))
)

