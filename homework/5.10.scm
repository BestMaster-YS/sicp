
;; 在这里新建机器语法
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
        ;; 新增递增指令，使寄存器的值+1
        ((eq? (car inst) 'incr)
         (make-incr inst machien labels ops pc))
        (else (error "Unknown instruction type -- ASSEMZBLE" inst))))

(define (make-inrc inst machine labels ops pc)
  (let ((target
         (get-register machine (incr-reg-name inst))))
    (lambda ()
      (set-contents! target (+ (get-contents target) 1))
      (advance-pc pc))))

(define (incr-reg-name inst)
  (cadr inst))