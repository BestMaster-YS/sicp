
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

