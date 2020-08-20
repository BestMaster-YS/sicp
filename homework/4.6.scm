(define (let? exp) (tagged-list? exp 'let))

(define (let-body exp) (caddr exp))
(define (let-inner-exps exp) (cadr exp))

(define (let->combination exp)
  ((make-lambda (map car (let-inner-exps exp))
                (let-body exp))
   (map cadr (let-inner-exps exp))))

(define (make-let var-list body)
  (list 'let var-list body))

;; eval
(define (eval exp env)
  ((cond ((self-evaluating? exp) exp)
         ((variable? exp) (look-up-variable-value exp env))
         ((quoted? exp) (text-of-quotation exp))
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
         ((let? exp) (eval (let->combination exp) env)
         ((application? exp)
          (apply (eval (operator exp) env)
                 (list-of-values (operands exp) env)))
         (else
          (error "Unknown expression type --- EVAL" exp)))))



