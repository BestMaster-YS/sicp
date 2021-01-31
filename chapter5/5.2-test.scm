(load "5.2-regsim-self.scm")

;;开始执行 gcd-machine

(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))

;; 设置初始值
(set-register-contents! gcd-machine 'a 206)

(set-register-contents! gcd-machine 'b 40)

(start gcd-machine)

(get-register-contents gcd-machine 'a)

(print-stack-statistics gcd-machine)


