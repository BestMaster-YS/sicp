(define (record-additional-message inst insts goto-regs stack-regs regs-from)
    (begin
        (if (member (car inst) insts)
            'exist
            (set! insts (cons inst-name insts)))
        (cond ((eq? (car inst) 'assign)
               ;; 存储 regs-from
               (set! regs-from (cons (cddr inst) regs-from)))
              ((eq? (car inst) 'goto)
               (if (member (car inst) goto-regs)
                   'exist
                   (set! goto-regs (cons (car inst) goto-regs))))
              ((eq? (car inst) 'save)
               (if (member (car inst) stack-regs)
                   'exist
                   (set! stack-regs (cons (car inst) stack-regs))))
              ((eq? (car inst) 'restore)
               (if (member (car inst) stack-regs)
                   'exist
                   (set! stack-regs (cons (car inst) stack-regs)))))))


(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        ;; 指令表
        (unique-insts '())
        ;; 保存入口点的表
        (use-goto-regs '())
        ;; 使用了 save 或 restore 的寄存器表
        (use-stack-regs '())
        ;; 使用 assign 的来源表
        (regs-from '())
        )
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register: " name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ;; 指令执行时可得到所有信息
                (record-additional-message (car insts) unique-insts use-gott-regs use-stack-regs regs-from)
                ((instruction-execution-proc (car insts)))
                (execute)))))
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
              ((eq? message 'get-instructions) unique-insts)
              ((eq? message 'get-goto-regs) use-goto-regs)
              ((eq? message 'get-stack-regs) use-stack-regs)
              ((eq? message 'get-regs-from) regs-from)
              (else
               (error "Unknown request -- MACHINE" message))))
      dispatch)))