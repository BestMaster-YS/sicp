(define (let*? exp) (tagged-list? exp 'let*))

(define (let*-values exp) (cadr exp))
(define (let*-body exp) (caddr exp))

(define (let*->nested-lets exp)
  (define (make-rest-lets vars body)
    (if (= (length vars) 0)
        body
        (make-let (car vars)
                  (make-rest-lets (cdr vars)
                                  body))))
  (make-rest-lets (let*-values exp)
                  (let*-body exp)))



