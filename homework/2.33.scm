
(load "util.scm")

(define nil '())

(define (map f sequence)
  (accmulate (lambda (x y) (cons (f x) y))
	     nil
	     sequence
  )
)

(map (lambda (x) (+ x 1)) (list 1 2 3))

(define (append list1 list2)
  (accmulate cons list2 list1)
)

(append (list 1 2 3) (list 4 5 6))

(define (length sequence)
  (accmulate (lambda (x y) (+ 1 y))
	     0
	     sequence))

(length (list 1 2 3))

