(load "5.5-machine-action.scm")

(compile-action
 '(define (factorial n)
     (if (= n 1)
         1
         (* (factorial (- n 1)) n)))
 'val
 'next)

