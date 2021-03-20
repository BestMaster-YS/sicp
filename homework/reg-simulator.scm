(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (show exp)
  (newline)
  (display exp))

;; one-dimensional tables
(define (lookup key table)
  (let ((record (assoc-self key (cdr tabel))))
    (if record
        (cdr record)
        false)))

(define (assoc-self key records)
  (cond ((null? records) false)
        ((equal? (caar records) key) (car records))
        (else
         (assoc-self key (cdr records)))))


(define (insert! key value table)
  (let ((record (assoc-self key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value)
                        (cdr table))))
    'ok))

(define (make-table) (list '*table*))

;; 二维表格存储断点信息
(define (lookup2d! key1 key2 table)
  (let ((subtable (assoc-self key1 (cdr table))))
    (if subtable
        (let ((record (assoc-self key2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert2d! key1 key2 value table)
  (let ((subtable (assoc-self key1 (cdr table))))
    (if subtable
        (insert! key2 value subtable)
        (set-cdr! table
                  (cons (list key1 (cons key2 value))
                        (cdr table))))
    'ok))

;; creating local state
(define (make-table-local)
  (let ((local-table (list '*local-table*)))
    (define (lookup key1 key2)
      (let ((subtable (assoc-self key1 (cdr local-table))))
        (if subtable
            (let ((record (assoc-self key2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))

    (define (insert key1 key2 value)
      (let ((subtable (assoc-self key1 (cdr local-table))))
        (if subtable
            (insert! key2 value subtable)
            (set-cdr! local-table
                      (cons (list key1 (cons key2 value))
                            (cdr local-table))))
        'ok))
    (define (dispatch op)
      (cond ((eq? op 'lookup) lookup)
            ((eq? op 'insert) insert)
            ((eq? op 'show) (lambda () (show local-table)))
            (else
             (error "Unknown operation: TABLE" op))))
    dispatch))

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
    (let ((assemble-value (assemble controller-text machine)))
      (let ((first-label (car assemble-value))
            (insts (cdr assemble-value)))
        (begin
          ((machine 'install-instructino-sequence) insts)
          (set-register-contents! machine 'inst-label first-label))))
    machine))

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else
             (error "Unknown request -- REGISTER" message))
            ))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

(define (make-stack)
  (let ((s '())
        ;; 增加监视机器执行的变量
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes  '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics) (print-statistics))
            (else
             (error "Unknown request -- STACK" message))))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

;; 基本机器1

(define (start machine)
  (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)


(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (inst-label (make-register 'inst-label)))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statisitics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag) (list 'inst-label inst-label)))
          (breakpoint (make-table-local))
          (next-process 'undefined))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (add-break-point label num)
        ((breakpoint 'insert) label num #t))
      (define (exist-break-point label num)
        ((breakpoint 'lookup) label num))
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register: " name))))
      (define (execute)
        (let ((insts (get-contents pc))
              (inst-label-value (get-contents inst-label)))
          (if (null? insts)
              'done
               (let ((inst-number-value (instruction-num (car insts))))
                 (if (exist-break-point inst-label-value inst-number-value)
                     (begin
                       (set! next-process (lambda ()
                                            ((instruction-execution-proc (car insts)))
                                            (execute)))
                       'breakpoint)
                     (begin
                       ((instruction-execution-proc (car insts)))
                       (execute)))))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instructino-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'proceed-machine) (next-process))
              ((eq? message 'set-breakpoint) add-break-point)
              (else
               (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (set-breakpoint machine label num)
  ((machine 'set-breakpoint) label num))

(define (proceed-machine machine)
  (machine 'proceed-machine))

;; assemble program

(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    ;; 将指令执行过程插入指令表中
                    (update-insts! insts labels machine)
                    ;; 将第一个 label 返回
                    (if (null? labels)
                        (cons 'none-label insts)
                        (cons (car (car labels)) insts)))
                  1))


;; 顺序扫描text中的元素，若是 symbol（标号）就加入 labels 中
;; 否则加入 insts 指令表中
(define (extract-labels text receive num)
  (if (null? text)
      (receive '() '())
      (let ((cur-inst (car text))
            (next-insts (cdr text)))
        (extract-labels next-insts
                        (lambda (insts labels)
                          (if (symbol? cur-inst)
                              ;; label
                              (receive insts
                                  (cons (make-label-entry cur-inst
                                                          insts)
                                        labels))
                              ;; insts
                              (receive (cons (make-instruction num cur-inst)
                                             insts)
                                  labels)))
                        (if (symbol? cur-inst)
                            1
                            (+ num 1))))))

;; update-insts!
(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instructions-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts)))

(define (instruction-num inst)
  (car inst))

(define (make-instruction num text)
  (list num text))

(define (instruction-text inst)
  (cadr inst))

(define (instruction-execution-proc inst)
  (caddr inst))

(define (set-instructions-execution-proc! inst proc)
  (set-cdr! inst (append (cdr inst) (list proc))))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))

;;  5.2.3 为指令生成执行过程

(define (make-execution-procedure inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMZBLE" inst))))


(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               ;; 为 op
               (make-operation-exp
                value-exp machine labels operations)
               ;; 为 const label reg
               (make-primitive-exp
                (car value-exp) machine labels))))
      ;; 查找寄存器和分析值表达式只需在汇编时执行一次
      ;; 执行指令时只是赋值和更新PC
      (lambda ()
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

;; test,branch,goto 指令

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))


(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        ;; 找到对应 label 的指令序列，赋值给 PC
        (let ((insts
               (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (begin
                  (set-contents! pc insts)
                  ;; 更新 label
                  (set-register-contents! machine 'inst-label (label-exp-label dest)))
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))


(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda ()
               (begin
                 (set-register-contents! machine 'inst-label (label-exp-label dest))
                 (set-contents! pc insts)))))
          ((register-exp? dest)
           ;; 当 goto 作用于寄存器时，reg 存储的东西已经在分析阶段处理完成，在 assign
           ;; 语句分析，已经得到结果，需要在 assign 语句时增加 label 信息
           ;; 5.5.7 加入编译代码后，reg 可能为指令集，为不破坏这里的代码结构，可将 compile-and-go 中的代码保持与书中一致
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
             (lambda ()
               (let ((reg-value (get-contents reg)))
                 (let ((label (car reg-value))
                       (insts (cdr reg-value)))
                   (begin
                     (set-contents! pc insts)
                     (set-register-contents! machine 'inst-label label)))))))
          (else
           (error "Bad GOTO instruction -- ASSEMBLE" inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

;; 其他指令

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))


(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))


(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

;; 子表达式的执行过程

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label labels
                              (label-exp-label exp))))
           ;; 将 label 信息一起返回
           (lambda () (cons (label-exp-label exp) insts))))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))


(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (make-primitive-exp e machine labels))
              (operations-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))

(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))

(define (operations-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))

(define (print-stack-statistics machine)
  ((machine 'stack) 'print-statistics))
