(load "util.scm")
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
         ((application? exp)
          (apply (eval (operator exp) env)
                 (list-of-values (operands exp) env)))
         (else
          (error "Unknown expression type --- EVAL" exp)))))

(define (expression-type exp) (car exp))
(define (expression-content exp) (cdr exp))

(define (eval exp env)
  (if (self-evaluating? exp)
      exp
      (let ((eval-proc (get 'eval (expression-type exp))))
        (eval-proc (expression-content exp)
                   env))))

(define (install-eval-packages)
  ;; variable
  (put 'eval 'variable?
       (lambda (exp env)
         (look-up-variable-value exp env)))
  ;; ...
  (put 'eval 'quote
       (lambda (exp env)
         (text-of-quotation exp)))
  'done)

