(load "../chapter5/5.2-regsim-self.scm")

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

;; a)

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
     (assign val (const 0))
     (save continue)
     count-leave-loop
     (test (op null?) (reg tree))
     (branch (label tree-done0))
     (test (op not-pair?) (reg tree))
     (branch (label tree-done1))
     child-left
     ;; 求值left
     ;; 先保存 tree
     (save tree)
     ;; 保存求 right 后续过程
     (assign continue (label child-right))
     (save continue)
     ;; 取出 left
     (assign tree (op car) (reg tree))
     ;; 开始求值 left
     (goto (label count-leave-loop))
     child-right
     ;; 弹出 tree
     (restore tree)
     (assign tree (op cdr) (reg tree))
     ;; 先保存求值 left 后的 val
     (assign left-val (reg val))
     (save left-val)
     (assign continue (label after-child))
     (save continue)
     ;; 开始求值 right
     (goto (label count-leave-loop))
     after-child
     (restore left-val)
     (assign val (op +) (reg val) (reg left-val))
     (restore continue)
     (goto (reg continue))
     tree-done0
     (assign val (const 0))
     (restore continue)
     (goto (reg continue))
     tree-done1
     (assign val (const 1))
     (restore continue)
     (goto (reg continue))
     done
     )))

;;(set-register-contents! count-leaves-machine  'tree '((1 1) (1 1)))

;;(start count-leaves-machine)

;;(get-register-contents count-leaves-machine 'val)


;; b)


(define (count-leaves-rec tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
          ((not (pair? tree)) (+ n 1))
          (else (count-iter (cdr tree)
                            (count-iter (car tree) n))))))

;; A情况是计算完 (car tree) 后计算 (cdr tree)
;; B情况是计算完 (count-iter (car tree) n) 之后，再进行父运算

(define count-leaves-rec-machine
  (make-machine
   '(tree n continue)
   (list (list 'null? null?) (list '+ +) (list 'car car) (list 'cdr cdr)
         (list 'not-pair? (lambda (v) (not (pair? v)))))
   '(init
     (assign continue (label done))
     (assign n (const 0))
     (save continue)
     count-leaves-loop
     (test (op null?) (reg tree))
     (branch (label null-tree))
     (test (op not-pair?) (reg tree))
     (branch (label leaf-tree))
     (save tree)
     (assign tree (op car) (reg tree))
     ;; 对 (count-iter (car tree) n) 求值
     (assign continue (label after-child))
     (save continue)
     (goto (label count-leaves-loop))
     after-child
     (restore tree)
     (assign tree (op cdr) (reg tree))
     (goto (label count-leaves-loop))
     null-tree
     (restore continue)
     (goto (reg continue))
     leaf-tree
     (assign n (op +) (reg n) (const 1))
     (restore continue)
     (goto (reg continue))
     done)))



(set-register-contents! count-leaves-rec-machine  'tree '((1 1) (1)))

(start count-leaves-rec-machine)

(get-register-contents count-leaves-rec-machine 'n)





