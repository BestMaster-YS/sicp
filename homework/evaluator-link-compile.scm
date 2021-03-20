(load "compile-origin.scm")
(load "explicit-control-evaluator.scm")

(define (compile-and-go expression)
  (let ((instructions
         (assemble
          (statements
           (compile expression 'val 'return))
          scm-machine)))
    (set! the-global-environment (setup-environment))
    (set-register-contents! scm-machine 'val instructions)
    (set-register-contents! scm-machine 'flag true)
    (start scm-machine)
    ))




