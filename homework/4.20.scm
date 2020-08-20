;; (a)

(define (letrec? exp)
  (tagged-list? exp 'letrec))

(define (letrec-bindings exp) (cadr exp))

(define (letrec-body) (cddr exp))





















