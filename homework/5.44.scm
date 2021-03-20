(load "compile.scm")
;; 在 5.38 新增的练习中，我们直接使用，operator 是否在 '(+ / * -) 来使用开放性代码提高机器效率
;; 现在利用 compile-time-environment 来判断 + / * - 是否被重新定义过，这里不考虑 (define + +) 这种重复定义的情况，若是被定义过则不使用开放式代码

(define (primitive-application? exp compile-time-environment)
  (let ((compile-addr (find-variable (car exp) compile-time-environment)))
    (and (eq? compile-addr 'not-found) (memq (car exp) '(- =)))))

(define (primitive-application-arbitrary? exp compile-time-environment)
  (let ((compile-addr (find-variable (car exp) compile-time-environment)))
    (and (eq? compile-addr 'not-found) (memq (car exp) '(+ *)))))

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
        ((primitive-application? exp compile-time-environment)
         (compile-primitive-application exp target linkage compile-time-environment))
        ((primitive-application-arbitrary? exp compile-time-environment)
         (compile-primitive-arbitrary-application exp target linkage compile-time-environment))
        ((application? exp)
         (compile-application exp target linkage compile-time-environment))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

;(define statement1
;  '(let ((+ (define (add x y) (+ x y))))
;     (+ 1 2)))
;
;(define content1
;  (compile
;   statement1
;   'val
;   'next
;   '()))
;
;(write-file "5.44-c1.scm" content1)
;














