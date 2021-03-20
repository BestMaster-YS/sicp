(load "5.40.scm")
;; 转换 let 为 lambda，将内部变量作为常规的 lambda 变量定义

(define (let? exp) (tagged-list? exp 'let))

(define (let-body exp) (cddr exp))
(define (let-inner-exps exp) (cadr exp))

(define (let->combination exp)
  (cons (make-lambda (map car (let-inner-exps exp))
                     (let-body exp))
        (map cadr (let-inner-exps exp))))

(define (compile exp target linkage compile-time-environment)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage compile-time-environment))
        ((quoted? exp) (compile-quoted exp target linkage compile-time-environment))
        ((variable? exp)
         (compile-variable exp target linkage compile-time-environment))
        ((assignment? exp)
         (compile-assignment exp target linkage compile-time-environment))
        ((definition? exp)
         (compile-definition exp target linkage compile-time-environment))
        ((if? exp) (compile-if exp target linkage compile-time-environment))
        ((let? exp) (compile (let->combination exp) target linkage compile-time-environment))
        ((lambda? exp) (compile-lambda exp target linkage compile-time-environment))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage
                           compile-time-environment))
        ((cond? exp) (compile (cond->if exp) target linkage compile-time-environment))
        ((primitive-application? exp)
         (compile-primitive-application exp target linkage compile-time-environment))
        ((primitive-application-arbitrary? exp)
         (compile-primitive-arbitrary-application exp target linkage compile-time-environment))
        ((application? exp)
         (compile-application exp target linkage compile-time-environment))
        (else
         (error "Unknown expression type -- COMPILE" exp))))


;(define statement1
;  '(let ((a 1))
;     (+ a 1))
;  )
;
;(define content1
;  (compile
;   statement1
;   'val
;   'next
;   '()))
;
;(write-file "5.43-c1.scm" content1)
;

