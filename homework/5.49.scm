(load "evaluator-link-compile.scm")
;; 需要的operatoins


(define (compile-and-assemble exp)
   (assemble
    (statements
     (compile exp 'val 'return))
    ec-comp-exec))

(define operations
  (append (list
           (list 'compile-and-assemble compile-and-assemble))
          eceval-operatons))

(define ec-comp-exec
  (make-machine
   '(exp env val continue proc argl unev compapp)
   operations
   '(read-eval-print-loop
     (perform (op initialize-stack))
     (perform (op prompt-for-input) (const ";;;EC-eval input::"))
     (assign exp (op read))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (assign val (op compile-and-assemble) (reg exp))
     (goto (reg val))
     print-result
     (perform (op print-stack-statisitics))
     (perform (op announce-output) (const ";;;EC-eval output::"))
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))
     )))


(start ec-comp-exec)
