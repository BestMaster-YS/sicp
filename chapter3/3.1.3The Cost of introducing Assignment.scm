(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define W (make-simplified-withdraw 5))

(W 5)
(W 5)
(W 5)

(define (make-decrementer balance)
  (lambda (amount) (- balance amount)))

;; samenes and change

;; pitfalls of impreative programming

(define (factorial n)
  (let ((counter 1)
        (product 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))


