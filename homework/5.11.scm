(save y)
(save x)
(restore y)

;; (a)
;; 保证相同数量的 save 和 restore 指令能永远保持 stack 为空

;; (b)

(define (make-stack)
  (let ((s '()))
    ;; 修改 push 参数，存储 name 和 value
    (define (push name value)
      (set! s (cons (cons name value) s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else
             (error "Unknown request -- STACK" message))))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack name value) ((stack 'push) name value))


(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      ;; 存储寄存器的 name 和 value
      (push stack reg (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (let ((reg-store (pop stack)))
        ((let ((reg-name (car reg-store))
               (reg-value (cadr reg-store)))
           (if (eq? reg-name reg)
               (lambda ()
                 (set-contents! reg (pop stack))
                 (advance-pc pc))
               (error "REG-NAME ERROR -- RESTORE"))))))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

;; (c)
;; 两个思路
;; 1：为所有的寄存器单独维护一份 stack，当 push 和 pop 执行，先找到对应的 stack，进行存储或取出值
;; 2：只有一份 stack，执行 pop 时遍历找到最近的 stack 值

;; 未完成
(define (make-stack)
  (let ((s '()))
    ;; 修改 push 参数，存储 name 和 value
    (define (push name value)
      (set! s (cons (cons name value) s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else
             (error "Unknown request -- STACK" message))))
    dispatch))




