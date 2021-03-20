(
    (env)
    (env proc argl continue val)
    (
        (assign proc (op make-compiled-procedure) (label entry4) (reg env))
        (goto (label after-lambda3))
        entry4
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env (op extend-environment) (const (+)) (reg argl) (reg env))
        (assign proc (op lexical-address-lookup) (const (0 . 0)) (reg env))
        (assign val (const 2))
        (assign argl (op list) (reg val))
        (assign val (const 1))
        (assign argl (op cons) (reg val) (reg argl))
        ;; 这里则是执行普通的 primitive-procedure 分支测试
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch7))
        compiled-branch6
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
        primitive-branch7
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
        (goto (reg continue))
        after-call5
        after-lambda3
        (assign val (op make-compiled-procedure) (label entry2) (reg env))
        (goto (label after-lambda1))
        entry2
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env (op extend-environment) (const (x y)) (reg argl) (reg env))
        (assign arg1 (op lexical-address-lookup) (const (0 . 0)) (reg env))
        (assign arg2 (op lexical-address-lookup) (const (0 . 1)) (reg env))
        (assign arg1 (op +) (reg arg1) (reg arg2))
        ;; 这里还是执行开放式的代码
        (assign val (op +) (reg arg1) (reg arg2))
        (goto (reg continue))
        after-lambda1
        (perform (op define-variable!) (const add) (reg val) (reg env))
        (assign val (const ok))
        (assign argl (op list) (reg val))
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch10))
        compiled-branch9
        (assign continue (label after-call8))
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
        primitive-branch10
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
        after-call8
    )
)