;; 求值器中已经有了 driver-loop 所以在显示控制求值器中只需要将编译好的求值运行起来
(load "5.50-eval.scm")
(load "evaluator-link-compile.scm")

(define eceval-operatons
  (append (list
           (list 'apply apply))
          eceval-operatons))

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
        (list 'apply apply)
        (list 'eq? eq?)
        (list 'pair? pair?)
        (list 'symbol? symbol?)
        (list 'number? number?)
        (list 'string? string?)
        (list 'cadr cadr)
        (list 'caddr caddr)
        (list 'caadr caadr)
        (list 'cddr cddr)
        (list 'not not)
        (list 'list list)
        (list 'map map)
        (list 'cadddr cadddr)
        (list 'set-car! set-car!)
        (list 'set-cdr! set-cdr!)
        (list 'length length)
        (list 'error error)
        (list 'display display)
        (list 'append append)
        (list 'read read)
        ))


(compile-and-go
 evaluator-exp)






