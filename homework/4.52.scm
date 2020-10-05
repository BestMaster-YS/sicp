(define (if-fail? exp) (tagged-list? exp 'if-fail))

(define (analyze-if-fail exp)
  (let ((fproc (analyze (cadr exp)))
        (sproc (analyze (caddr exp))))
    (lambda (env succeed fail)
      (fproc
       env
       (lambda (val fail2)
         (succeed val fail))
       (lambda ()
         (sproc env succeed fail))))))


