
(define (addr-frame lexical-address)
  (car lexical-address))

(define (addr-displacement lexical-address)
  (cdr lexical-address))

(define (create-empty-runtime-env) '())

(define (lexical-address-lookup address env)
  (let ((frame (list-ref env (addr-frame address))))
    (let ((value (list-ref frame (addr-displacement address))))
      (if (or (not value) (equal? value '*unassigned*))
          (error "VALUE UNASSIGNED, COMPILE-TIME")
          value))))


(define (list-ref-set! alist idx value)
  (if (= idx 0)
      (set-car! alist value)
      (list-ref-set! (cdr alist) (- idx 1) value)))

(define (lexical-address-set! address env value)
  (let ((frame (list-ref env (addr-frame address)))
        (offset (addr-displacement address)))
    (list-ref-set! frame offset value)))

(define (indexof a listA)
  (define (iter curL i)
    (if (null? curL)
        -1
        (let ((cur (car curL)))
          (if (eq? a cur)
              i
              (iter (cdr curL) (+ i 1))))))
  (iter listA 0))

(define (find-variable variable compile-time-env)
  (define (iter rest-frame idx)
    (if (null? rest-frame)
        'not-found
        (let ((inner-idx (indexof variable (car rest-frame))))
          (if (eq? inner-idx -1)
              (iter (cdr rest-frame) (+ idx 1))
              (cons idx inner-idx)))))
  (iter compile-time-env 0))



