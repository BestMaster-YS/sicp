(load "5.2-regsim-self.scm")
(load "5.4-scm-machine-lib.scm")
(load "io-lib.scm")
(load "5-5compile-ans.scm")

;; 需要的operatoins
(define eceval-operatons
  (list
   ;;primitive Scheme operations
   (list 'read read)
   (list 'cons cons)

   ;;operations in syntax.scm
   (list 'self-evaluating? self-evaluating?)
   (list 'quoted? quoted?)
   (list 'text-of-quotation text-of-quotation)
   (list 'variable? variable?)
   (list 'assignment? assignment?)
   (list 'assignment-variable assignment-variable)
   (list 'assignment-value assignment-value)
   (list 'definition? definition?)

   (list 'definition-variable definition-variable)
   (list 'definition-value definition-value)
   (list 'lambda? lambda?)
   (list 'lambda-parameters lambda-parameters)
   (list 'lambda-body lambda-body)
   (list 'if? if?)
   (list 'if-predicate if-predicate)
   (list 'if-consequent if-consequent)
   (list 'if-alternative if-alternative)
   (list 'begin? begin?)
   (list 'begin-actions begin-actions)
   (list 'last-exp? last-exp?)
   (list 'first-exp first-exp)
   (list 'rest-exps rest-exps)
   (list 'application? application?)
   (list 'operator operator)
   (list 'operands operands)
   (list 'no-operands? no-operands?)
   (list 'first-operand first-operand)
   (list 'rest-operands rest-operands)

   ;;operations in eceval-support.scm
   (list 'true? true?)
   (list 'false? false?)
   (list 'make-procedure make-procedure)
   (list 'compound-procedure? compound-procedure?)
   (list 'procedure-parameters procedure-parameters)
   (list 'procedure-body procedure-body)
   (list 'procedure-environment procedure-environment)
   (list 'extend-environment extend-environment)
   (list 'lookup-variable-value lookup-variable-value)
   (list 'set-variable-value! set-variable-value!)
   (list 'define-variable! define-variable!)
   (list 'primitive-procedure? primitive-procedure?)
   (list 'apply-primitive-procedure apply-primitive-procedure)
   (list 'prompt-for-input prompt-for-input)
   (list 'announce-output announce-output)
   (list 'user-print user-print)
   (list 'empty-arglist empty-arglist)
   (list 'adjoin-arg adjoin-arg)
   (list 'last-operand? last-operand?)
   (list 'no-more-exps? no-more-exps?)	;for non-tail-recursive machine
   (list 'get-global-environment get-global-environment)
   (list 'make-compiled-procedure make-compiled-procedure)
   (list 'compiled-procedure-env compiled-procedure-env)
   (list 'list list)
   (list 'compiled-procedure-entry compiled-procedure-entry)))

(define the-global-environment (setup-environment))


(define (compound-compiled-instruction-with-machine instruction)
  (append
   (append
    '(start
      (perform (op initialize-stack))
      (perform (op prompt-for-input) (const ";;;EC-eval input::")))
    instruction)
   '(print-result
     (perform (op print-stack-statisitics))
     (perform (op announce-output) (const ";;;EC-eval output::"))
     (perform (op user-print) (reg val)))
   ))

(define (compile-action exp target linkage)
  (define machine
    (make-machine
     '(exp env val continue proc argl unev)
     eceval-operatons
     (compound-compiled-instruction-with-machine
      (statements (compile exp target linkage)))))
  (start machine))






