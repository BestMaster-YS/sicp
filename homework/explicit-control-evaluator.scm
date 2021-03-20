(load "reg-simulator.scm")
(load "scm-machine.scm")

;; 需要的operatoins
(define eceval-operatons
  (list
   ;;primitive Scheme operations
   (list 'read read)

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
   (list 'list list)
   (list 'cons cons)
   (list 'false? false?)

   ;;operations in eceval-support.scm
   (list 'true? true?)
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
   (list 'compiled-procedure? compiled-procedure?)
   (list 'compiled-procedure-entry compiled-procedure-entry)
   (list 'compiled-procedure-env compiled-procedure-env)
   (list 'make-compiled-procedure make-compiled-procedure)))

(define the-global-environment (setup-environment))

(define (start-eceval machine)
  (set! the-global-environment (setup-environment))
  (set-register-contents! machine 'flag false)
  (start machine))

(define scm-machine
  (make-machine
   '(exp env val continue proc argl unev compapp)
   eceval-operatons
   '(;;当在外部已经设置好 flag 寄存器的值，则跳转到 external-entry
     (assign compapp (label compound-apply))
     (branch (label external-entry))
     read-eval-print-loop
     (perform (op initialize-stack))
     (perform (op prompt-for-input) (const ";;;EC-eval input::"))
     (assign exp (op read))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (label eval-dispatch))
     external-entry
     (perform (op initialize-stack))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (reg val))
     print-result
     (perform (op print-stack-statisitics))
     (perform (op announce-output) (const ";;;EC-eval output::"))
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))
     eval-dispatch
     (test (op self-evaluating?) (reg exp))
     (branch (label ev-self-eval))
     (test (op variable?) (reg exp))
     (branch (label ev-variable))
     (test (op quoted?) (reg exp))
     (branch (label ev-quoted))
     (test (op assignment?) (reg exp))
     (branch (label ev-assignment))
     (test (op definition?) (reg exp))
     (branch (label ev-definition))
     (test (op if?) (reg exp))
     (branch (label ev-if))
     (test (op lambda?) (reg exp))
     (branch (label ev-lambda))
     (test (op begin?) (reg exp))
     (branch (label ev-begin))
     (test (op application?) (reg exp))
     (branch (label ev-application))
     (goto (label unknown-expression-type))
     ;; 简单表达式求值
     ev-self-eval
     (assign val (reg exp))
     (goto (reg continue))
     ev-variable
     (assign val (op lookup-variable-value) (reg exp) (reg env))
     (goto (reg continue))
     ev-quoted
     (assign val (op text-of-quotation) (reg exp))
     (goto (reg continue))
     ev-lambda
     (assign unev (op lambda-parameters) (reg exp))
     (assign exp (op lambda-body) (reg exp))
     (assign val (op make-procedure)
             (reg unev) (reg exp) (reg env))
     (goto (reg continue))
     ;; 过程应用求值
     ev-application
     (save continue)
     (save env)
     (assign unev (op operands) (reg exp))
     (save unev)
     ;; 先求值运算符
     (assign exp (op operator) (reg exp))
     (assign continue (label ev-appl-did-operator))
     (goto (label eval-dispatch))
     ;; 求值运算对象初始化
     ev-appl-did-operator
     (restore unev)
     (restore env)
     (assign argl (op empty-arglist))
     ;; 将 proc 设置为求值运算符产生出的过程
     (assign proc (reg val))
     (test (op no-operands?) (reg unev))
     ;; 没参数则直接执行
     (branch (label apply-dispatch))
     (save proc)
     ;; 取出一个参数进行计算求值
     ev-appl-operand-loop
     (save argl)
     (assign exp (op first-operand) (reg unev))
     (test (op last-operand?) (reg unev))
     (branch (label ev-appl-last-arg))
     (save env)
     (save unev)
     (assign continue (label ev-appl-accumulate-arg))
     (goto (label eval-dispatch))
     ;; 求值完一个参数后，积累到 argl 寄存器中，并从 unev 中删除
     ev-appl-accumulate-arg
     (restore unev)
     (restore env)
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (assign unev (op rest-operands) (reg unev))
     (goto (label ev-appl-operand-loop))
     ev-appl-last-arg
     (assign continue (label ev-appl-accum-last-arg))
     (goto (label eval-dispatch))
     ev-appl-accum-last-arg
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (restore proc)
     ;; 将参数应用于过程
     (goto (label apply-dispatch))
     ;; 过程应用
     apply-dispatch
     (test (op primitive-procedure?) (reg proc))
     (branch (label primitive-apply))
     (test (op compound-procedure?) (reg proc))
     (branch (label compound-apply) (reg proc))
     (test (op compiled-procedure?) (reg proc))
     (branch (label compiled-apply))
     (goto (label unknown-procedure-type))
     primitive-apply
     (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
     (restore continue)
     (goto (reg continue))
     compound-apply
     (assign unev (op procedure-parameters) (reg proc))
     (assign env (op procedure-environment) (reg proc))
     (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
     (assign unev (op procedure-body) (reg proc))
     (goto (label ev-sequence))
     compiled-apply
     (restore continue)
     (assign val (op compiled-procedure-entry) (reg proc))
     (goto (reg val))
     ev-begin
     (assign unev (op begin-actions) (reg proc))
     (save continue)
     (goto (label ev-sequence))
     ev-sequence
     (assign exp (op first-exp) (reg unev))
     (test (op last-exp?) (reg unev))
     (branch (label ev-sequence-last-exp))
     (save unev)
     (save env)
     (assign continue (label ev-sequence-continue))
     (goto (label eval-dispatch))
     ev-sequence-continue
     (restore env)
     (restore unev)
     (assign unev (op rest-exps) (reg unev))
     (goto (label ev-sequence))
     ev-sequence-last-exp
     (restore continue)
     (goto (label eval-dispatch))
     ev-if
     (save exp)
     (save env)
     (save continue)
     (assign continue (label ev-if-decide))
     (assign exp (op if-predicate) (reg exp))
     (goto (label eval-dispatch))
     ev-if-decide
     (restore continue)
     (restore env)
     (restore exp)
     (test (op true?) (reg val))
     (branch (label ev-if-consequent))
     ev-if-alternative
     (assign exp (op if-alternative) (reg exp))
     (goto (label eval-dispatch))
     ev-if-consequent
     (assign exp (op if-consequent) (reg exp))
     (goto (label eval-dispatch))
     ;; 赋值和定义
     ev-assignment
     (assign unev (op assignment-variable) (reg exp))
     (save unev)
     (assign exp (op assignment-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-assignment-l))
     (goto (label eval-dispatch))
     ev-assignment-l
     (restore continue)
     (restore env)
     (restore unev)
     (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))
     ev-definition
     (assign unev (op definition-variable) (reg exp))
     (save unev)
     (assign exp (op definition-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-definition-l))
     (goto (label eval-dispatch))
     ev-definition-l
     (restore continue)
     (restore env)
     (restore unev)
     (perform (op define-variable!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))
     unknown-expression-type
     (assign val (const unknown-expression-type-error))
     (goto (label signal-error))
     unknown-procedure-type
     (assign val (const unknown-procedure-type-error))
     (goto (label signal-error))
     signal-error
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))
     )))





