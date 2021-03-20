(
    (env)
    (env proc argl continue val)
    (
        (assign proc (op make-compiled-procedure) (label entry2) (reg env))
        (goto (label after-lambda1))
        entry2
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env (op extend-environment) (const (a)) (reg argl) (reg env))
        (assign arg1 (op lexical-address-lookup) (const (0 . 0)) (reg env))
        (assign arg2 (const 1))
        (assign arg1 (op +) (reg arg1) (reg arg2))
        (assign val (op +) (reg arg1) (reg arg2))
        (goto (reg continue))
        after-lambda1
        (assign val (const 1))
        (assign argl (op list) (reg val))
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch5))
        compiled-branch4
        (assign continue (label after-call3))
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
        primitive-branch5
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
        after-call3
    )
)