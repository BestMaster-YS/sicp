(load "util.scm")

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b)))
)

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b)))
)

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b)))
)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b)))
)

(define (inc n) (+ n 1)) 
(define (sum-cube a b)
  (sum cube a inc b)
)

(sum-cube 1 10)

(define (identity a) a)
(define (sum-integers a b)
  (sum identity a inc b)
)

(sum-integers 1 10)

(define (pi-sums a b)
  (define (pi-term a)
    (/ 1.0 (* a (+ a 2)))
  )
  (define (pi-next a)
    (+ a 4)
  )
  (sum pi-term a pi-next b)
)

(* 8 (pi-sums 1 1000))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx)
)


(integral cube 0 1 0.0001)
