(load "util.scm")


(define (make-account balance idcard)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'idcard) idcard)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) serializer)
            (else (error "Unknown request: MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (serializer-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer))
        (idcard1 (account1 'idcard))
        (idcard2 (account2 'idcard)))
    ((if (> idcard1 idcard2)
         ((serializer2 (serializer1 exchange)))
         ((serializer1 (serializer2 exchange))))
     account1
     account2)))


