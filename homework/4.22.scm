(define (analyze-let exp)
  (let ((bindings (let-inner-exps exp))
        (body (let-body exp)))
    (lambda (env)
      ((make-lambda (map car bindings)
                    body)
       (map cdr bindings)))))


