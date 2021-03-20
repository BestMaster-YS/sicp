(load "5-5compile-ans.scm")
(load "5.4explicit-control-evaluator.scm")

;; 由于在 regsim-self 中改造 assemble 函数，返回的结构为 (cons first-label inst)
;; 虽然修改 assemble 函数，但同时我们修改了 make-branch 函数，所以保持 compile-and-go 不变

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

(compile-and-go
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n))))



