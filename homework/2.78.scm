;; 修改 type-tag contents attach-tag 函数

(define (type-tag datum)
  (cond ((number? datum) datum)
        ((pair? datum) (car datum))
        (else
         (error "Bad tagged datum -- CONTENTS" datum))))

(define (attach-tag type-tag z)
  (if (number? z)
      z
      (cons type-tag z)))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else
         (error "Bad tagged datum -- CONTENTS" datum))))

