
((unless? exp) (eval (unless->if exp) env))

(define (unless? exp) (tagged-list? exp 'unless))

(define (unless-condition exp) (cadr exp))

; (define (unless-usual-value exp) (caddr exp))

(define (unless-exceptional-value exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (unless-usual-value exp) (caddr exp))

(define (unless->if exp)
  (make-if (unless exp) (unless-exceptional-value exp) (unless-usual-value exp)))






