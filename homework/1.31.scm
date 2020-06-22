(load "util.scm")

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b)))
)

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result)))
  )
  (iter a 1)
)

(define (factrial a b)
  (product identity a inc b)
)

(factrial 1 5)


(define (pi-product n)
  (define (term i)
    (if (odd? i)
        (/ (+ i 1) (+ i 2))
        (/ (+ i 2) (+ i 1)))
  )
  (* 4.0 (product term 1 inc n))
)

(pi-product 10000)

