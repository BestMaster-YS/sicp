;; action :: var val vars vals
;; pre-check :: var val
;; not-find-action :: env

(define (scan-env pre-check action not-find-in-frame-action env)
  (define (scan vars vals)
    (cond ((null? vars)
           (not-find-in-frame-action env ))
          ((pre-check (car vars) (car vals))
           (action (car vars) (car vals) vars vals))
          (else
           (scan (cdr vars) (cdr vals)))))
  (scan (frame-variables (first-frame env))
        (frame-variables (first-frame env))))


(define (lookup-variable-value var env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (scan-env
       (lambda (frame-var frame-val)
         (eq? frame-var var))
       (lambda (frame-var frame-val frame-vars frame-vals)
         frame-val)
       (lambda (env)
         (lookup-variable-value var (enclosing-environment env)))
       env)))

(define (set-variable-value! var val env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable: SET-VARIABLE-VALUE" var)
      (scan-env
       (lambda (frame-var frame-val)
         (eq? frame-var var))
       (lambda (frame-var frame-val frame-vars frame-vals)
         (set-car! frame-vals val))
       (lambda (env)
         (set-variable-value! var (enclosing-environment env)))
       env)))

(define (define-variable! var val env)
  (scan-env
   (lambda (frame-var frame-val)
     (eq? var frame-var))
   (lambda (frame-var frame-val frame-vars frame-vals)
     (set-car! frame-vals val))
   (lambda (env)
     (add-binding-to-frame! var val (first-frame env)))
   env))


