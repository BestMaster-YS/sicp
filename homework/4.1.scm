;; left -> right
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first-value (eval (first-operand exps) env)))
        (cons first-value
              (list-of-values (rest-operads exps) env)))))

;; right -> left
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((next (list-of-values (rest-operads exps) env)))
        (cons (eval (first-operand exps) env)
              next))))



