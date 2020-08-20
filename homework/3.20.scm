(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch op)
    (cond ((eq? op 'car ) x)
          ((eq? op 'cdr ) y)
          ((eq? op 'set-car! ) set-x!)
          ((eq? op 'set-cdr! ) set-y!)
          (else
            (error "undefined operation: CONS" op))))
  dispatch)

(define (car m) (m 'car ))
(define (cdr m) (m 'cdr ))
(define (set-car! m new-value)
  ((m 'set-car! ) new-value) m)
(define (set-cdr! m new-value)
  ((m 'set-cdr! ) new-value) m)

(define x (cons 1 2))
(define z (cons x x))
(set-car! (car z) 17)
(car x)


; http://community.schemewiki.org/?sicp-ex-3.20
