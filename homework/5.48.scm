(load "evaluator-link-compile.scm")

(define (compile-and-run expression)
  (let ((instructions
         (assemble
          (statements
           (compile expression 'val 'return))
          scm-machine)))
    (set-register-contents! scm-machine 'val instructions)
    (set-register-contents! scm-machine 'flag true)
    (start scm-machine)
    ))

;; 注入到 primitive 函数过程列表中
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        ;;above from book -- here are some more
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '= =)
        (list '/ /)
        (list '> >)
        (list '< <)
        (list 'compile-and-run compile-and-run)
        ))

(start-eceval scm-machine)


