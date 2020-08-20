(define a 10)

(parallel-execute
 (lambda () (set! x (* x x)))
 (lambda () (set! x (+ x 1))))


(display x)

(define b 10)

(define s (make-serializer))
(parallel-execute
 (s (lambda () (set! b (* b b))))
 (s (lambda () (set! b (+ b 1)))))


(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            (else (error "Unknown request: MAKE-ACCOUNT"
                         m))))
    dispatch))


(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))


(define (make-account balance)
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
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) serializer)
            (else (error "Unknown request: MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (serializer-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

;; 串行化的实现


(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serializer-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serializer-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 ;; 无限排队
                 (the-mutex 'acquire)))
            ((eq? m 'release)
             (clear! cell))))))

(define (clear! cell)
  (set-car! cell false))


(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))
;; test-and-set! 必须是原子性的操作，当一个进程访问互斥元时，发现为 false，那就必须在其他进程访问之前，设置为 true













