(define x '((a b) c d))

(define y '(e f))

(set-car! x y)

(display x)

; (define (cons x y)
;   (let ((new (get-new-pair)))
;     (set-car! new x)
;     (set-cdr! new y)
;     new))


;; Sharing and identity


(define x (list 'a 'b))
(define z1 (cons x x))

(define z2 (cons (list 'a 'b) (list 'a 'b )))

(define (set-to-wow! x) (set-car! (car x) 'wow ) x)

(set-to-wow! z1)
(set-to-wow! z2)

;; mutation is just assignment

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


(define a (cons 1 2))

(car a)
(set-car! a 2)
