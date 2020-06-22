(define (cons a b)
	(define (dispatch i)
  	(cond ((= i 0) a)
          ((= i 1) b)
          (else
            (error "Argument not 0 or 1 -- CONS" i)))
  )
  dispatch
)

(define (car z) (z 0))
(define (cdr z) (z 1))