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

(define (make-join account password new-password)
  (lambda (action pwd)
    (if (eq? pwd new-password)
        (lambda (amount)
          ((account action password) amount))
        (error "Incorrect password"))))

(define peter-acc (make-account 1000 'open-sesame))

((peter-acc 'withdraw 'open-sesame) 100)
(define paul-acc (make-join peter-acc 'open-sesame 'rosebud))

((paul-acc 'withdraw 'rosebud) 100)


