(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs))
           ((car procs) env))
          (else
           ((car procs) env) (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs) (error "Empty sequence: ANALYZE"))
    (lambda (env) (execute-sequence procs env))))

;; optimize_eval 是将body内所有的execution放置在一个函数内执行，只用调用一次
;; execute-sequence
;; Alyssa P. Hacker 的程序还是递归的执行 execute-sequence


