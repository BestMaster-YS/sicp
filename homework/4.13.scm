(define (unbound? exp) (tagged-list exp 'unbound))
(define (unbind-variable exp env) (make-unbound (cadr exp) env))

(define (make-unbound var env)
  (scan-env
   (lambda (frame-var frame-val)
     (eq? var frame-var))
   (lambda (frame-var frame-val frame-vars frame-vals)
     (begin
       (set! frame-vars (cdr frame-vars))
       (set! frame-vals (cdr frame-vals))))
   (lambda (env)
     'ok)
   env))

