(load "util.scm")

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b)
)


(* 8 (pi-sum 1 1000))

;; let 

;; 用内部 define
(define (f x y)
  (define (helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b))
  )
  (helper (+ 1 (* x y))
          (- 1 y))
)
; 用 lambda
(define (f x y)
  (lambda (a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (+ 1 (* x y))
  (- 1 y)
)

;; 用 let

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b)))
)
