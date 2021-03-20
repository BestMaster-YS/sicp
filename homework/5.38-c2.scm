(
    (env)
    (val)
    (
        (assign val (op make-compiled-procedure) (label entry13) (reg env))
        (goto (label after-lambda12))
        entry13
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env (op extend-environment) (const (x y)) (reg argl) (reg env))
        (assign arg1 (const 3))
        (assign arg2 (op lookup-variable-value) (const x) (reg env))
        (assign arg1 (op +) (reg arg1) (reg arg2))
        (assign arg2 (op lookup-variable-value) (const y) (reg env))
        (assign val (op +) (reg arg1) (reg arg2))
        (goto (reg continue))
        after-lambda12
        (perform (op define-variable!) (const plus3) (reg val) (reg env))
        (assign val (const ok))
        ))