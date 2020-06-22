(load "util.scm")

(define test (list 1 2 3))

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(reverse test)

(define (reverse sequence)
  (fold-left (lambda (x y) (append (list y) x)) nil sequence))

(reverse test)
