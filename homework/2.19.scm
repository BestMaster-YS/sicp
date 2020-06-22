;; 重写 coins-change
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (coins-exchange amount coins-list)
  (cond ((= amount 0) 1)
        ((< amount 0) 0)
        ((null? coins-list) 0)
        (else
          (let ((cur-coins (car coins-list))
                (next (cdr coins-list)))
            (+ (coins-exchange (- amount cur-coins) coins-list)
               (coins-exchange amount next)))))
)

(coins-exchange 100 (list 25 50 10 5 1))
