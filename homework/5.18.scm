(load "../chapter5/5.2-regsim-self.scm")

(define (display-register name old-value new-value)
  (newline)
  (display name)
  (display ": ")
  (display old-value)
  (display " -> ")
  (display new-value))

(define (make-register name)
  (let ((contents '*unassigned*)
        (trace    #f))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (if trace
                   (display-register name contents value)
                   'no-trace)
               (set! contents value)))
            ((eq? message 'trace)
             (set! trace #t))
            (else
             (error "Unknown request -- REGISTER" message))
            ))
    dispatch))

(define (trace-register register)
  (register 'trace))

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

(trace-register (get-register gcd-machine 'a))

(start gcd-machine)

(get-register-contents gcd-machine 'a)


