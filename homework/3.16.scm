(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;; 存在指向相同的 cons 导致计数不对

(define cycle (cons 1 (cons 2 (cons 3 '()))))

(set-cdr! (last-pair cycle) cycle)

(define two (cons 1 (cons 2 '())))

(define four (cons two (cdr two)))

(define three (cons 1 (cons 2 (cons 3 '()))))

(count-pairs four)
