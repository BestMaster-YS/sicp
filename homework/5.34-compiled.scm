(
    (env)
    (val)
    (
        ;; 组装 val 为  compiled-procedure 入口点为 entry2 环境为 env
        (assign val (op make-compiled-procedure) (label entry2) (reg env))
        (goto (label after-lambda1))
        entry2
        ;; 进入 factorial 内部
        (assign env (op compiled-procedure-env) (reg proc))
        ;; 形参和实参
        (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
        ;; 构造内部 compiled-procedure
        (assign val (op make-compiled-procedure) (label entry7) (reg env))
        (goto (label after-lambda6))
        entry7
        (assign env (op compiled-procedure-env) (reg proc))
        ;; 绑定形参和实参 (product counter) (1  1)
        (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
        (save continue)
        (save env)
        (assign proc (op lookup-variable-value) (const >) (reg env))
        ;; 获取 n
        (assign val (op lookup-variable-value) (const n) (reg env))
        (assign argl (op list) (reg val))
        ;; counter
        (assign val (op lookup-variable-value) (const counter) (reg env))
        (assign argl (op cons) (reg val) (reg argl))
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch22))
        compiled-branch21
        (assign continue (label after-call20))
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
        primitive-branch22
        ;; > counter n
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
        after-call20
        (restore env)
        (restore continue)
        (test (op false?) (reg val))
        (branch (label false-branch9))
        true-branch10
        ;; 返回结果
        (assign val (op lookup-variable-value) (const product) (reg env))
        (goto (reg continue))
        false-branch9
        ;; 进行下一次迭代的而准备
        (assign proc (op lookup-variable-value) (const iter) (reg env))
        ;; 在进行表达式的应用时什么都可以改变，必须全部保存
        (save continue)
        ;; 先保存 proc (iter)
        (save proc)
        ;; 保存环境
        (save env)
        (assign proc (op lookup-variable-value) (const +) (reg env))
        (assign val (const 1))
        (assign argl (op list) (reg val))
        (assign val (op lookup-variable-value) (const counter) (reg env))
        (assign argl (op cons) (reg val) (reg argl))
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch16))
        compiled-branch15
        (assign continue (label after-call14))
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
        primitive-branch16
        ;; + counter 1
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
        after-call14
        ;; (counter)
        (assign argl (op list) (reg val))
        ;; 恢复环境找到最新的 counter 和 product
        (restore env)
        (save argl)
        ;; 计算 product
        (assign proc (op lookup-variable-value) (const *) (reg env))
        (assign val (op lookup-variable-value) (const product) (reg env))
        (assign argl (op list) (reg val))
        (assign val (op lookup-variable-value) (const counter) (reg env))
        (assign argl (op cons) (reg val) (reg argl))
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch13))
        compiled-branch12
        (assign continue (label after-call11))
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
        primitive-branch13
        ;; product = product * counter
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
        after-call11
        ;; 重点：恢复 argl, proc, continue
        (restore argl)
        (assign argl (op cons) (reg val) (reg argl))
        (restore proc)
        (restore continue)
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch19))
        compiled-branch18
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
        primitive-branch19
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
        (goto (reg continue))
        after-call17
        after-if8
        after-lambda6
        ;; 绑定 iter 为 entry7 的 compiled 函数 
        (perform (op define-variable!) (const iter) (reg val) (reg env))
        ;; 结束
        (assign val (const ok))
        ;; 调用 iter
        (assign proc (op lookup-variable-value) (const iter) (reg env))
        ;; 计算参数 (1 1)
        (assign val (const 1))
        (assign argl (op list) (reg val))
        (assign val (const 1))
        (assign argl (op cons) (reg val) (reg argl))
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch5))
        compiled-branch4
        ;; 获取 iter 的 entry
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
        primitive-branch5
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
        (goto (reg continue))
        after-call3
        after-lambda1
        ;; 将 factorial 存在env中 compiled-procedure
        (perform (op define-variable!) (const factorial) (reg val) (reg env))
        (assign val (const ok))
        ;; 因为只是定义一个函数，所以指令会从开头直接跳转到这里
        ;; 使用 
        ))