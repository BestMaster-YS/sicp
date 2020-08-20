(define (eval exp env)
  ((cond ((self-evaluating? exp) exp)
         ((variable? exp) (look-up-variable-value exp env))
         ((quoted? exp) (text-of-quotation exp))
         ((application? exp)
          (apply (eval (operator exp) env)
                 (list-of-values (operands exp) env)))
         ((assignment? exp) (eval-assignment exp env))
         ((definition? exp) (eval-definition exp env))
         ((if? exp) (eval-if exp env))
         ((lambda? exp)
          (make-procedure (lambda-parameters exp)
                          (lambda-body exp)
                          env))
         ((begin? exp)
          (eval-sequence (begin-actions exp) env))
         ((cond? exp) (eval (cond->if exp) env))
         (else
          (error "Unknown expression type --- EVAL" exp)))))

;; (a)  遇到 (define a 3) 语句会进行过程应用操作，而环境中没有define函数
;; (b) 改写过程应用

(define (application? exp) (tagged-list? exp 'call))

(define (operator exp) (cadr exp))

(define (operands exp) (cddr exp))


