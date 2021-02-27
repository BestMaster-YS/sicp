
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;; a 最大深度为 10
;; b

;; 0  29
;; 1  64  -- 35
;; 2  99  -- 35
;; 3  134 -- 35
;; 4  169 -- 35
;; 5  204 -- 35

;; y = 35n + 29


(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))


;; 1  total-16   max-8
;; 2        48       13
;; 3        80       18
;; 4        112      23


;;         最大深度          压栈次数
;; 迭代       10             y = 35n + 29
;; 递归       5n + 3         y = 32n - 16
;;














