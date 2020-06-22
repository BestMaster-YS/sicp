(define test (cons (list 1 2) (list 3 4)))

(length test)

;; pair? 检查是否为序对

(define (count-leaves items)
  (cond ((null? items) 0)
        ((not (pair? items)) 1)
        (else (+ (count-leaves (car items))
                 (count-leaves (cdr items)))))
)

(count-leaves test)
;; 对树的映射
(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else
          (cons (scale-tree (car tree) factor)
                (scale-tree (cdr tree) factor))))
)

(define tree1 (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(scale-tree tree1 10)

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
               (if (pair? sub-tree)
                   (scale-tree sub-tree factor)
                   (* sub-tree factor)))
       tree)
)

(scale-tree tree1 10)

