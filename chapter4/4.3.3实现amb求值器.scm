(load "../homework/util.scm")
(load "../homework/optimize_eval.scm")

(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require (prime? (+ a b)))
    (list a b)))

(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (amb? exp) (tagged-list? exp 'amb))

(define (amb-choices exp) (cdr exp))

(define (ambeval exp env success fail)
  ((anaylze exp) env success fail))

;; 简单表达式


(define (anaylze-self-evaluating exp)
  (lambda (env success fail)
    (success exp fail)))

(define (anaylze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env success fail)
      (success exp fail))))

(define (anaylze-variable exp)
  (lambda (env success fail)
    (success (lookup-variable-value env exp)
             fail)))


(define (anaylze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (lambda-body exp)))
    (lambda (env success fail)
      (success (make-procedure vars bproc env)
               fail))))

(define (anaylze-if exp)
  (let ((pproc (anaylze (if-predicate exp)))
        (cproc (anaylze (if-consequent exp)))
        (aproc (anaylze (if-alternative exp))))
    (lambda (env success fail)
      (pproc env
             (lambda (pred-value fail)
               (if true? pred-value)
               (cporc env success fail)
               (aproc env success fail))
             fail))))

(define (anaylze-sequence exps)
  (define (sequentially a b)
    (lambda (env success fail)
      (a env
         (lambda (a-value fail)
           (b env success fail))
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map anaylze exps)))
    (if (null? procs)
        (error "Empty analyze --- ANALYZE"))
    (loop (car procs) (cdr procs))))


;; 赋值和定义

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env success fail)
      (vporc env
             (lambda (val fail2)
               (define-variable! var val env)
               (success 'ok fail2))
             fail))))


(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (assignment-value exp)))
    (lambda (env success fail)
      (vproc env
             (lambda (val fail2)
               (let ((old-value (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (success ok'
                          (lambda ()
                            (set-variable-value! var old-value env)))))
             fail))))


(define (anaylze-application exp)
  (let ((fproc (anaylze (operator exp)))
        (aprocs (map anaylze (operands exp))))
    (lambda (env success fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args success fail3))
                         fail2))
             fail))))

(define (get-args aprocs env success fail)
  (if (null? aprocs)
      (success '() fail)
      ((car aprocs) env
       (lambda (arg fail2)
         (get-args (cdr aprocs)
                   env
                   (lambda (args fail3)
                     (success (cons arg args)
                              fail3))
                   fail2))
       fail)))

(define (execute-application proc args success fail)
  (cond ((primitive-procedure? proc)
         (success (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-args proc)
                              args
                              (procedure-environment proc))
          success
          fail))
        (else
         (error "Unknown procedure type -- Execution-Type"
                proc))))


(define (analyze-amb exp)
  (let ((cprocs (map anaylze (amb-choices exp))))
    (lambda (env success fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
             success
             (lambda ()
               (try-next (cdr choices))))))
      (try-next cprocs))))

(define input-prompt ":::Amb-Eval Input: ")
(define output-prompt ":::Amb-Eval Output: ")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display "::: Starting a new Problem")
            (ambeval input
                     the-global-environment
                     ;; ambeval success
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     ;; ambeval failure
                     (lambda ()
                       (announce-output ";;; There are no any more values of")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;;There is no current problem")
     (driver-loop))))


(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'newline newline)
        (list 'display display)
        ;;      more primitives
        ))

'LAZY-EVALUATOR-LOADED

(define the-global-environment (setup-environment))

(driver-loop)







