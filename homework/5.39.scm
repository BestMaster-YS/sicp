(load "table.scm")

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


(define env (create-empty-runtime-env))

(lexical-address-set! (cons 3 2) env 2)

(lexical-address-lookup (cons 3 2) env)

