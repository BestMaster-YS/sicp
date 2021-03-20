;; 5.5.1 编译器结构
(load "5.4-syntax.scm")

;; compile 完成最高层的分派，

;; target 为目标寄存器
(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp)
         (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))


;; 目标和连接
;; 指令序列和堆栈的使用

(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

;; 5.5.2 表达式的编译

;; 连接代码的编译

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue) '()
                                    '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence '() '()
                                    `((goto (label ,linkage)))))))

;; 使用 preserving 保护 continue 寄存器，因为 return 连接将需要 continue 寄存器。
(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
               instruction-sequence
               (compile-linkage linkage)))

;; 简单表达式的编译

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence '() (list target)
                               `(assign ,target (const ,exp)))))

(define (compile-quoted exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence '() (list target)
                               `((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence '(env) (list target)
                               `((assign ,target
                                         (op look-variable-value)
                                         (const ,exp)
                                         (reg env))))))

(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next)))
    (end-with-linkage
     linkage
     (preserving '(env)
                  get-value-code
                  (make-instruction-sequence '(env val) (list target)
                                              `((perform (op set-variable-value!)
                                                         (const ,var)
                                                         (reg val)
                                                         (reg env))
                                                (assign ,target (const ok))))))))

(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp) 'val 'next)))
    (end-with-linkage
     linkage
     (preserving '(env)
                  get-value-code
                  (make-instruction-sequence '(env val) (list target)
                                              `((perform (op define-variable!)
                                                          (const ,var)
                                                          (reg val)
                                                          (reg env))
                                                 (assign ,target (const ok))))))))

(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string->symbol
   (string-append (symbol->string name)
                  (number->string (new-label-number)))))

(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           ;; 识别真分支后的连接处理
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next))
            (c-code
             (compile
              (if-consequent exp) target consequent-linkage))
            (a-code
             (compile (if-alternative exp) target linkage)))
        (preserving '(env continue)
                     p-code
                     (append-instruction-sequence
                      (make-instruction-sequence '(val) '()
                                                  `((test (op false?) (reg val))
                                                    (branch (label ,f-branch))))
                      (parallel-instruction-sequences
                       (append-instruction-sequence t-branch c-code)
                       (append-instruction-sequence f-branch a-code))
                      after-if))))))


;; Compiling sequences

(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage)
      (preserving
       '(env continue)
       (compile (first-exp seq) target 'next)
       (compile-sequence (rest-exps seq) target linkage))))


;; Compiling lambda expressions

(define (compile-lambda exp target linkage)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequence
       (tack-on-instruction-sequence
        (end-with-linkage
         lambda-linkage
         (make-instruction-sequence
          '(env) (list target)
          `((assign ,target
                    (op make-compiled-procedure)
                    (label ,proc-entry)
                    (reg env)))))
        (compile-lambda-body exp proc-entry))
       after-lambda))))

(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequence
     (make-instruction-sequence '(env proc argl) '(env)
                                `(,proc-entry
                                  ;; 构造一个过程环境
                                  (assign env
                                          (op compiled-procedure-env)
                                          (reg proc))
                                  ;; 将变量存储到环境中
                                  (assign env
                                          (op extend-environment)
                                          (const ,formals)
                                          (reg argl)
                                          (reg env))))
     ;; 在新的过程环境中进行求值，并将结果赋值给 val
     (compile-sequence (lambda-body exp) 'val 'return))))

;; Compiling Combinations

(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
        (operands-codes
         (map (lambda
                  (operand) (compile operand 'val 'next))
              (operands exp))))
    (preserving '(env continue)
                proc-code
                (preserving '(proc continue)
                            (construct-arglist operands-codes)
                            (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  ;; 先进行对求值 operands 的代码取反，保证能从 last -> first
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
                                   '(assign argl (const ())))
        (let ((code-to-get-last-arg
               (append-instruction-sequence
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                                           '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              ;; 保护 env 寄存器
              (preserving '(env)
                          code-to-get-last-arg
                          (code-to-get-rest-args
                           (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         ;; 保护 argl 寄存器
         (preserving '(argl)
                     (car operand-codes)
                     (make-instruction-sequence '(val argl) '(argl)
                                                '((assign argl
                                                          (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        (code-for-next-arg
         (preserving '(env)
                     code-for-next-arg
                     (code-to-get-rest-args (cdr operand-codes)))))))

;; Applying procedure

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequence
       (make-instruction-sequence '(proc) '()
                                  `((test (op primitive-procedure?) (reg proc))
                                    (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequence
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (append-instruction-sequence
         primitive-branch
         (end-with-linkage linkage
                           (make-instruction-sequence
                            '(proc argl)
                            (list target)
                            `((assign ,target
                                      (op apply-primitive-procedure)
                                      (reg proc)
                                      (reg argl)))))))
       after-call))))

;; Applying compiled procedures

(define all-regs '(env proc val argl continue))

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
                                    `((assign continue (label ,linkage))
                                      ;; 获取编译好的过程应用的入口
                                      (assign val (op compiled-procedure-entry)
                                              (reg proc))
                                      (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
                                      `((assign continue (label ,proc-return))
                                        (assign val (op compiled-procedure-entry)
                                                (reg proc))
                                        (goto (reg val))
                                        ,proc-return
                                        (assign ,target (reg val))
                                        (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence
          '(proc continue)
          all-regs
          '((assign val (op compiled-procedure-entry)
                    (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val))
              (eq? linkage 'return))
         (error "return linkage, target not val: COMPILE"
                target))))
;; 5.5.4 Combining Instruction Sequence

(define (registers-needed s)
  (if (symbol? s) '() (car s)))

(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))

(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))

(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

;; append-instruction-sequence

(define (append-instruction-sequence . seqs)
  (define (append-2-sequence seq1 seq2)
    (make-instruction-sequence
     (list-union
      (registers-needed seq1)
      (list-difference (registers-needed seq2)
                       (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequence
         (car seqs)
         (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))


(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))

;; preserving

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequence seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
                        (make-instruction-sequence
                         (list-union (list first-reg)
                                     (registers-needed seq1))
                         (list-difference (registers-modified seq1)
                                          (list first-reg))
                         (append `((save ,first-reg))
                                 (statements seq1)
                                 `((restore ,first-reg))))
                        seq2)
            (preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq)
           (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1)
           (statements seq2))))

;; 5.5.5 An Example of Compiled Code















