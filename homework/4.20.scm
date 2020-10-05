;; (a)

(define (letrec? exp)
  (tagged-list? exp 'letrec))

(define (letrec-bindings exp) (cadr exp))

(define (letrec-body) (cddr exp))

(define (declare-variables exp)
  (map (lambda (x) (list (car x) '*unassigned*))
       (letrec-bindings exp)))

(define (set-variables exp)
  (map (lambda (x) (list 'set! (car x) (cadr x)))
       (letrec-bindings exp)))

(define (letrec->let-combination exp)
  (list 'let (declare-variables exp)
        (make-begin (append (set-variables exp) (letrec-body exp)))))

;;
;letrec (var value) (body) -> (lambda (var) (set! var value) (body))(*unassigned*)
;let (var value) (body) -> (lambda (var) (body))(value)
; letrec 




















