(load "../chapter5/5.2-regsim-self.scm")

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))


(define count-leaves-machine
  (make-machine
   ;; register-names
   '(continue tree val left-val)
   ;; operations
   (list (list 'car car) (list 'cdr cdr) (list 'null? null?)
         (list 'not-pair? (lambda (v) (not (pair? v))))
         (list '+ +))
   '(start
     (assign continue (label done))
     (save continue)
     count-leave-loop
     (test (op null?) (reg tree))
     (branch (label tree-done0))
     (test (op not-pair?) (reg tree))
     (branch (label tree-done1))
     child-left
     ;; 求值left
     ;; 先取出 right 保存
     (assign tree (op cdr) (reg tree))
     (save tree)
     ;; 保存求 right 后续过程
     (assign continue (label child-right))
     (save continue)
     ;; 取出 tree
     (assign tree (op car) (reg tree))
     ;; 开始求值 left
     (goto (label count-leave-loop))
     child-right
     ;; 弹出 right
     (restore tree)
     ;; 先保存求值 left 后的 val
     (assign left-val (reg val))
     (save left-val)
     (assign continue (label after-child))
     (save continue)
     ;; 开始求值 right
     (goto (label count-leave-loop))
     after-child
     (assign val (op +) (reg val) (reg left-val))
     (goto (reg continue))
     tree-done0
     (assign val (const 0))
     (goto (reg continue))
     tree-done1
     (assign val (const 1))
     (goto (reg continue))
     done
     )))

(define tree
  (cons
   (cons 1 (cons 1 1))
   (cons 1 (cons 1 1))))

(set-register-contents! count-leaves-machine  'tree tree)

(start count-leaves-machine)

(get-register-contents count-leaves-machine 'val)







