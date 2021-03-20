(load "reg-simulator.scm")
(load "scm-machine.scm")


(define fact-fib
  (make-machine
   ;; register names
   '(continue n val)
   ;; operations
   (list (list '- -) (list '+ +)  (list '< <) (list 'print-stack-statisitics) (list 'announce-output announce-output)
         (list 'user-print user-print) (list '= =))
   '(controller
     (assign continue (label fact-done))
     fact-loop
     (test (op <) (reg n) (const 2))
     (branch (label base-case))
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)
     (assign n (op -) (reg n) (const 1))
     (goto (label fact-loop))
     afterfib-n-1
     (restore n)
     (restore continue)
     (assign n (op -) (reg n) (const 2))
     (save continue)
     (assign continue (label afterfib-n-2))
     (save val)
     (goto (label fact-loop))
     afterfib-n-2
     (assign n (reg val))
     (restore val)
     (restore continue)
     (assign val (op +) (reg val) (reg n))
     (goto (reg continue))
     base-case
     (assign val (reg n))
     (goto (reg continue))
     fact-done
     (perform (op print-stack-statisitics))
     (perform (op announce-output) (const ";;;EC-eval output::"))
     (perform (op user-print) (reg val))
     )))


;; 设置初始值
(set-register-contents! fact-fib 'n 5)

(start fact-fib)




