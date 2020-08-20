(load "util.scm")

(define A (make-connector))
(define B (make-connector))
(define C (make-connector))

(define (average a b c)
  (let ((d (make-connector))
        (e (make-connector)))
    (adder a b d)
    (multiplier d e c)
    (constant (/ 1 2) e)))


(probe "A temp" A)
(probe "B temp" B)
(probe "C temp" C)
(average A B C)

(set-value! A 7 'user)
(set-value! B 8 'user)



