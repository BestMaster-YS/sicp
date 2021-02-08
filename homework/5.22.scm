;;(load "util.scm")
(load "../chapter5/5.2-regsim-self.scm")

(define (last-pair items)
  (let ((cur (car items))
        (next (cdr items)))
    (if (null? next)
        cur
        (last-pair next)))
  )

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define append-machine
  (make-machine
   '(x y res continue)
   (list (list 'car car) (list 'cdr cdr) (list 'null? null?)
         (list 'cons cons) (list 'display display))
   '(init
     (assign continue (label done))
     (save continue)
     append-loop
     (test (op null?) (reg x))
     (branch (label null-table))
     (save x)
     (assign continue (label after-child-append))
     (save continue)
     (assign x (op cdr) (reg x))
     (goto (label append-loop))
     after-child-append
     (restore x)
     (assign x (op car) (reg x))
     (assign res (op cons) (reg x) (reg res))
     (perform (op display) (reg res))
     (restore continue)
     (goto (reg continue))
     null-table
     (assign res (reg y))
     (restore continue)
     (goto (reg continue))
     done)))

(set-register-contents! append-machine 'x '(1))
(set-register-contents! append-machine 'y '(1 2 1))

(start append-machine)

(get-register-contents append-machine 'res)



