
(define (make-operation-exp exp machine labels operation)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                ;;
                (if (label-exp? e)
                    (error "CAN'NOT LABEL-EXPRESSION")
                    (make-primitive-exp e machine labels)))
              (operations-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))


