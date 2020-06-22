;; 递归
(define (pascal row col)
  (cond ((= col 1) 1)
        ((= row col) 1)
        (else (+ (pascal (- row 1) col)
                 (pascal (- row 1) (- col 1)))))
)

(pascal 5 2)


(define (factorial n)
    (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product)
                   (+ counter 1)
                   max-count)))
;; 迭代
(define (pascal row col)
  (/ (factorial row)
     (* (factorial col)
        (factorial (- row col))))
)

(pascal 564 12)
