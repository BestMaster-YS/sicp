(load "evaluator-link-compile.scm")
;; 使编译后的代码能调用解释性过程

;; 我们通过 compapp 寄存器保存 compound-apply 入口点
;; 需要先将 compound-apply 所需参数准备好

;; (assign argl)
;; (assign proc)

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        ;; 增加解释器中 compound-branch
        (compound-branch (make-label 'compound-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (append-instruction-sequences
        (make-instruction-sequence '(proc) '()
                                   `((test (op compound-procedure?) (reg proc))
                                     (branch (label ,compound-branch))))
        (make-instruction-sequence '(proc) '()
                                   `((test (op primitive-procedure?) (reg proc))
                                     (branch (label ,primitive-branch)))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compound-branch
         (compound-proc-appl target compiled-linkage))
        (parallel-instruction-sequences
         (append-instruction-sequences
          compiled-branch
          (compile-proc-appl target compiled-linkage))
         (append-instruction-sequences
          primitive-branch
          (end-with-linkage linkage
                            (make-instruction-sequence '(proc argl)
                                                       (list target)
                                                       `((assign ,target
                                                                 (op apply-primitive-procedure)
                                                                 (reg proc)
                                                                 (reg argl))))))))
       after-call))))


;; 调用 compound-apply, linkage !== next, 在 compile-procedure-call 中，已经将 next 替换为 after-call
;; target -> val, linkage !-> return

;; 在 (goto (reg compapp)) 前 (save continue)
;; 参考 compile 中 ev-begin 分支的做法
;; apply-dispatch 会在 ev-sequence-last-exp 时 (restore continue) 所以需要提前 (save continue)

(define (compound-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
                                    `((assign continue (label ,linkage))
                                      (save continue)
                                      (goto (reg compapp)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
                                      `((assign continue (label ,proc-return))
                                        (save continue)
                                        (goto (reg compapp))
                                        ,proc-return
                                        (assign ,target (reg val))
                                        (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
                                    '((save continue)
                                      (goto (reg compapp)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target))))

(compile-and-go
 '(define (test x y)
    (plus x y)))





