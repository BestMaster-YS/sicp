(load "util.scm")


(define (RC-circuit R C dt)
  (lambda (i-stream v0)
    (add-streams (scale-stream R i-stream)
                 (integral (scale-stream i-stream (/ 1 C))
                           v0
                           dt))))



