(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(withdraw 20)

(withdraw 20)

;; 重写 withdraw ，让 balance 称为 withdraw 的局部变量

(define withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (> balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(withdraw 20)

(define (make-withdraw balance)
  (lambda (amount)
    (if (> balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define w1 (make-withdraw 100))

(w1 50)
(w1 20)

;; 创建能提款和存款的银行账户

(define (make-account balance)
  (define (withdraw amount)
     (if (> balance amount)
         (begin (set! balance (- balance amount))
               balance)
         "Insufficient funds"))
  (define (deposit amount)
    (begin (set! balance (+ balance amount))
           balance))
  (define (dispatch action)
    (cond ((eq? action 'withdraw ) withdraw)
          ((eq? action 'deposit ) deposit)
          (else
            (error "Unknown action --- MAKE-ACCOUNT" action))))
  dispatch)

(define liubin (make-account 100000000000))

((liubin 'withdraw ) 100)
