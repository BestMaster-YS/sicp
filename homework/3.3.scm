;; 设置密码
(define (make-account balance password)
  (define (withdraw amount)
     (if (> balance amount)
         (begin (set! balance (- balance amount))
               balance)
         "Insufficient funds"))
  (define (deposit amount)
    (begin (set! balance (+ balance amount))
           balance))
  (define (dispatch action pwd)
    (if (eq? pwd password)
        (cond ((eq? action 'withdraw ) withdraw)
              ((eq? action 'deposit ) deposit)
              (else
                (error "Unknown action --- MAKE-ACCOUNT" action)))
        (error "Incorrect password")))
  dispatch)


(define liubin (make-account 100000000000 '123456 ))

((liubin 'withdraw '456789 ) 100)
