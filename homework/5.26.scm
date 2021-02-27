
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

