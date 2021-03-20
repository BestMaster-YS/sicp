(
 (env)
 (env proc argl continue val)
 (
    (assign proc (op make-compiled-procedure) (label entry2) (reg env))
    (goto (label after-lambda1))
    entry2
    (assign env (op compiled-procedure-env) (reg proc))
    (assign env (op extend-environment) (const (x y)) (reg argl) (reg env))
    (assign val (op make-compiled-procedure) (label entry4) (reg env))
    (goto (reg continue))
    entry4
    (assign env (op compiled-procedure-env) (reg proc))
    (assign env (op extend-environment) (const (a b c d e)) (reg argl) (reg env))
    (assign proc (op make-compiled-procedure) (label entry6) (reg env))
    (goto (label after-lambda5))
    entry6
    (assign env (op compiled-procedure-env) (reg proc))
    (assign env (op extend-environment) (const (y z)) (reg argl) (reg env))
    (assign arg1 (op lexical-address-lookup) (const #[constant 12 #x2]) (reg env))
    (assign arg2 (op lexical-address-lookup) (const #[constant 12 #x2]) (reg env))
    (assign arg1 (op *) (reg arg1) (reg arg2))
    (assign arg2 (op lexical-address-lookup) (const #[constant 12 #x2]) (reg env))
    (assign val (op *) (reg arg1) (reg arg2))
    (goto (reg continue))
    after-lambda5
    (assign arg1 (op lexical-address-lookup) (const #[constant 12 #x2]) (reg env))
    (assign arg2 (op lexical-address-lookup) (const #[constant 12 #x2]) (reg env))
    (assign arg1 (op +) (reg arg1) (reg arg2))
    (assign arg2 (op lexical-address-lookup) (const #[constant 12 #x2]) (reg env))
    (assign val (op +) (reg arg1) (reg arg2))
    (assign argl (op list) (reg val))
    (assign arg1 (op lexical-address-lookup) (const #[constant 12 #x2]) (reg env))
    (assign arg2 (op lexical-address-lookup) (const #[constant 12 #x2]) (reg env))
    (assign arg1 (op *) (reg arg1) (reg arg2))
    (assign arg2 (op lexical-address-lookup) (const #[constant 12 #x2]) (reg env))
    (assign val (op *) (reg arg1) (reg arg2))
    (assign argl (op cons) (reg val) (reg argl))
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch9))
    compiled-branch8
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    primitive-branch9
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    (goto (reg continue))
    after-call7
    after-lambda3
    after-lambda1
    (assign val (const 4))
    (assign argl (op list) (reg val))
    (assign val (const 3))
    (assign argl (op cons) (reg val) (reg argl))
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch12))
    compiled-branch11
    (assign continue (label after-call10))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    primitive-branch12
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    after-call10
    )
)
