(define (while? exp)
  (tagged-list? exp 'while))

(define (while-predicate exp) (cadr exp))
(define (while-body exp) (caddr exp))

(define (make-procedure-definition name parameters body)
  (cons 'define (cons (cons name parameters) body)))
(define (make-procedure-application procedure arguments)
  (cons procedure arguments))

(define (while->combination exp)
  (define (while->procedure-def procedure-name)
    (make-procedure-definition
     precedure-name
     '()
     (make-if
      (while-predicate exp)
      (sequence->exp
       (append (while-body exp)
               (make-procedure-application precedure-name '()))))))
  (make-procedure-application
   (make-lambda
    '()
    (list (while->procedure-def 'while-procedure)
          (make-procedure-application 'while-procedure '())))
   '()))

((lambda ()
   (define (while-procedure)
     (if (< i 100)
         (begin
           (newline)
           (display i)
           (set! (+ i 1))
           (while-procedure))))
   (while-procedure)))
