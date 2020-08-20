(load "util.scm")

(define (c+ c1 c2 )
  (let ((r (make-connector)))
    (adder c1 c2 r)
    r))

(define (c* c1 c2)
  (let ((r (make-connector)))
    (multiplier c1 c2 r)
    r))

(define (c/ c1 c2)
  (let ((r (make-connector)))
    ;; 除法：a / b = c -> c * b = a
    (adder c2 r c1)
    r))

(define (cv v)
  (define ((r (make-connector)))
    (constant v r)
    r))

(define (celsius-fahrenheit-conveter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-conveter C))

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

(set-value! C 25 'user)
