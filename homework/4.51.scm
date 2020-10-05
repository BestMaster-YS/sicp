(define (permanent-set? exp)
  (tagged-list? exp 'permanent-set!))

(define (analyze-pernamenant-set expr)
  (let ((var (assignment-variable expr))
        (vproc (analyze (assignment-value expr))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (set-variable-value! var val env)
               (succeed 'ok  fail2))
             fail))))

