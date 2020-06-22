(load "util.scm")

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp val) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
          (error "unknown expression type -- DERIV" exp))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else
          ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;;(a)

;;(b)

(define install-sum-package
  (define (addend z) (car z))
  (define (augend z) (cadr z))

  (define (make-sum x y)
    (cond ((=number? x 0) y)
          ((=number? y 0) x)
          ((and (number? x) (number? y)) (+ x y))
          (else
            (attach-tag '+ (cons x y)))))

  (put 'addend '+ addend)
  (put 'augend '+ augend)
  (put 'deriv '+
    (lambda (exp var)
      (make-sum (deriv (addend exp) var)
                (deriv (augend exp) var))))
  'done
  )

(define (make-sum x y)
    ((get 'make-sum '+) x y))

(define (addend sum)
    ((get 'addend '+) (contents sum)))

(define (augend sum)
    ((get 'augend '+) (contents sum)))


(define install-product-package
  (define (multiplier z) (car z))
  (define (multiplicand z) (cdr z))
  
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else
          (attach-tag '* (cons m1 m2)))))
  
  (put 'multiplier '* multiplier)
  (put 'multiplicand '* multiplicand)
  (put 'make-product '* make-product)

  (put 'deriv '*
    (lambda (exp var)
      (make-sum (make-product (multiplier exp)
                              (deriv (multiplicand exp) var))
                (make-product (multiplicand exp)
                              (deriv (multiplier exp) var)))))
  'done
  )

(define (make-product x y)
  ((get 'make-product '* ) x y))

(define (multiplier p)
  ((get 'multiplier '* ) (contents p)))

(define (multiplicand p)
  ((get 'multiplicand '* ) (contents p)))


;; (c) 乘幂


(define (install-exponentiation-package)
  (define (base exp)
    (car exp))
  (define (exponent exp)
    (cdr exp))
  (define (make-exponentiation base n)
    (cond ((= n 0) 0)
	  ((= n 1) base)
	  (else
	   (attach-tag '** (cons base n)))))
  (put 'base '** base)
  (put 'exponent '** exponent)
  (put 'make-exponentiation '** make-exponentiation)
  (put 'deriv '**
       (lambda (exp var)
	 (let ((n (exponent exp))
	       (u (base exp)))
	   (make-product
	    n
	    (make-product
	     (make-exponentiation
	      u
	      (- n 1))
	     (deriv u var))))))
)





