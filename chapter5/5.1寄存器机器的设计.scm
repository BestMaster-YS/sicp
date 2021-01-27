;; gcd

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; 控制器汇编级语言

(controller
 gcd-loop
 (assign a (op read))
 (assign b (op read))
 test-b
 (test (op =) (reg b) (const 0))
 (branch (label gcd-done))
 (assign t (op rem) (reg a) (reg b))
 (assign a (reg b))
 (assing b (reg t))
 (goto (label test-b))
 gcd-done
 (perform (op print) (reg a))
 (got (label gcd-loop)))

(define (remainder n d)
  (if (< n d)
      n
      (remainder (- n d) d)))

;; 将 remainder 用更基本过程抽象

(controller test-b
            (test (op =) (reg b) (const 0))
            (branch (label gcd-done))
            (assign t (reg a))
            rem-loop
            ;; rem 过程
            (test (op <) (reg t) (reg b))
            (branch (label rem-done))
            (assign t (op -) (reg t) (reg b))
            (goto (label rem-loop))
            rem-done
            (assign a (reg b))
            (assign b (reg t))
            (goto (label test-b))
            gcd-done)

;; Subroutines

;; 5.1.4 采用堆栈实现递归

(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

(controller
 ;; 存储最终结果后续程序
 (assign continue (label fact-done))
 fact-loop
 (test (op =) (reg n) (const 1))
 (branch (label base-case))
 ;; 存储 continue 和 n 到栈中，在处理完成 (factorial (- n 1)) 子程序后，使用
 (save continue)
 (save n)
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
 (assign val (op *) (reg n) (reg val))
 ;; 进行后续动作：结束或者是再次结束子程序（本身也为子程序）
 (goto (reg continue))
 base-case
 (assign val (const 1))
 (goto (reg continue))
 fact-done)

;; 双重递归

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(controller
 (assign continue (label fib-done))
 fib-loop
 (test (op <) (reg n) (const 2))
 (branch (label immediate-answer))
 ;; 保存完成 fib(n) 后后续操作，并开始 fib(n - 1)
 (save continue)
 (assign continue (label afterfib-n-1))
 (save n)
 (assign n (op -) (reg n) (const 1))
 (goto (label fib-loop))
 afterfib-n-1
 (restore n)
 ;;(restore continue)
 ;; 开始计算 fib(n - 2)
 (assign n (op -) (reg n) (const 2))
 ;;(save continue)
 (assign continue (label afterfib-n-2))
 (save val)
 (goto (label fib-loop))
 afterfib-n-2
 (assign n (reg val))
 (restore val)
 (restore continue)
 (assign val
         (op +) (reg val) (reg n))
 (goto (reg continue))
 immediate-answer
 (assign val (reg n))
 (goto (reg continue))
 fib-done)



