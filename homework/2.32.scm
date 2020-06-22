;(load "util.scm")

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (sub) (append (list (car s)) sub))  rest))))
)

(subsets (list 1 2 3))
