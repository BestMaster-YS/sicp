(load "util.scm")

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))


(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)


(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z

(cdr x)
(define w (append! x y))

w
(a b c d)
(cdr x)



