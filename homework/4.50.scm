;; 可以在直接改变choices内部的数据顺序，达到随机访问，也可以在 analyze-ramb 内部获取choices时进行随机访问

(define random-init 42)

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (rand-update x)
  (define a 13)
  (define b 17)
  (define m 19)
  (remainder (+ (* a x) b) m))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env success fail)
      ;; 实现不同的 try-next 达到目的
      (define (try-next choices)
        (if (null? choices)
            (fail)
            (let ((ran-choices (random-choices choices)))
              (let ((cur-choice (car ran-choices))
                    (rest-choices (cdr ran-choices)))
                (cur-choice
                 env
                 success
                 (lambda ()
                   (try-next rest-choices)))))))
      (try-next cprocs))))

(define (random-choices choices)
  (define (iter rest prev i cur)
    (if (= i cur)
        (cons (car rest) (append prev (cdr rest)))
        (iter (cdr rest) (cons (car rest) prev) i (+ cur 1))))
  (if (null? choices)
      (error "no choices")
      (iter choices
            '()
            (remainder (rand) (length choices))
            0)))
