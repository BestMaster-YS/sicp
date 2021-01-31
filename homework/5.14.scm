(load "../chapter5/5.2-regsim-self.scm")

(define (print-stack-depth value)
  (newline)
  (display "stack-depth: ")
  (display value))

(define fact-machine
  (make-machine
   ;; register names
   '(continue n val count stack-depth stack-max-depth)
   ;; operations
   (list (list '- -) (list '= =) (list '* *) (list '< <)
         (list 'print print-stack-depth) (list 'max max)
         (list '+ +))
   '(controller
     (assign n (reg count))
     (assign stack-depth (const 0))
     (assign stack-max-depth (const 0))
     ;; 初始赋值 1
     (assign val (const 1))
     ;; 存储最终结果后续程序
     (assign continue (label fact-done))
     ;; 额外存储一个寄存器使之 n 的值并且递增计算
     (assign count (op +) (reg count) (const 1))
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     ;; 存储 continue 和 n 到栈中，在处理完成 (factorial (- n 1)) 子程序后，使用
     (save continue)
     (save n)
     (assign stack-depth (op +) (reg stack-depth) (const 2))
     (assign stack-max-depth (op max) (reg stack-depth) (reg stack-max-depth))
     ;; n--
     (assign n (op -) (reg n) (const 1))
     ;; 记录后续操作
     (assign continue (label after-fact))
     ;; 进行子程序
     (goto (label fact-loop))
     after-fact
     ;; 完成子程序后从栈中恢复子程序父级状态
     (restore n)
     (restore continue)
     (assign stack-depth (op -) (reg stack-depth) (const 2))
     (assign val (op *) (reg n) (reg val))
     ;; 进行后续动作：结束或者是再次结束子程序（本身也为子程序）
     (goto (reg continue))
     base-case
     (assign val (const 1))
     (goto (reg continue))
     fact-done
     ;; 打印栈
     (perform (op print) (reg stack-max-depth))
     ;; 判断 count 小于 10
     (test (op <) (reg n) (const 10))
     ;; 跳转到 controller 初始化处
     (branch (label controller))
     )))


;; 设置初始值
(set-register-contents! fact-machine 'count 1)

(start fact-machine)

