; 将表和符号标记作为数据看待

(define a 1)
(define b 2)
(define c 3)

(list a b)

(list 'a 'b)

(car '(a b c))

(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'apple '(pear banana prune))
(memq 'apple '(x (apple sauce) y pear apple prune))

