(define (or? exp) (tagged-list? exp 'or))
(define (and? exp) (tagged-list? exp 'and))

(define (first-condition exp) (car exp))
(define (rest-conditions exp) (cdr exp))

(define (eval-or conditions env)
  (cond ((null? conditions) false)
        ((eval (first-condition conditions) env) true)
        (else
         (eval-or (rest-conditions conditions) env))))

(define (eval-or conditions env)
  (cond ((null? conditions) true)
        ((not (eval (first-condition conditions) env)) false)
        (else
         (eval-or (rest-conditions conditions) env))))

