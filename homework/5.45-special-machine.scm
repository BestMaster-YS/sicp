(load "reg-simulator.scm")
(load "scm-machine.scm")


(define fact-machine
  (make-machine
   ;; register names
   '(continue n val)
   ;; operations
   (list (list '- -) (list '* *) (list 'print-stack-statisitics) (list 'announce-output announce-output)
         (list 'user-print user-print) (list '= =))
   '(controller
     (assign continue (label fact-done))
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
     after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val))
     (goto (reg continue))
     base-case
     (assign val (const 1))
     (goto (reg continue))
     fact-done
     (perform (op print-stack-statisitics))
     (perform (op announce-output) (const ";;;EC-eval output::"))
     (perform (op user-print) (reg val))
     )))


;; 设置初始值
(set-register-contents! fact-machine 'n 5)

(start fact-machine)




