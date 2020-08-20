(load "util.scm")

(define (definition-let? exp)
  (and (tagged-list? exp 'let)
       (= (length exp) 4)))


(define (definition-let-vars exp) (caddr exp))

(define (definition-let-body exp) (cadddr exp))

(define (definition-let->combination exp)
  ((make-lambda (map car (definition-let-vars exp))
                (definition-let-body exp))
   (map cadr (definition-let-vars exp))))

(define (let? exp) (tagged-list? exp 'let))

(define (let-body exp) (caddr exp))
(define (let-inner-exps exp) (cadr exp))

(define (normal-let->combination exp)
  ((make-lambda (map car (let-inner-exps exp))
                (let-body exp))
   (map cadr (let-inner-exps exp))))

(define (let->combination exp)
  (if (definition-let? exp)
      (definition-let->combination exp)
      (normal-let->combination exp)))

