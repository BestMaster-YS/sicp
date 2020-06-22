(load "util.scm")

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* h k))))
  (define (term i)
    (cond ((or (= i 0) (= i n)) (y i))
          ((odd? i) (* 4 (y i)))
          ((even? i) (* 2 (y i))))
  )
  (* (/ h 3) (sum term 0 inc n))
)

(simpson-integral cube 0 1 1000)
