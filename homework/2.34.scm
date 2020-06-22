(load "util.scm")

(define (horner-eval x coefficient-sequence)
  (accmulate (lambda (this-coeff higher-item) (+ this-coeff
						 (* higher-item x)))
	     0
	     coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))


