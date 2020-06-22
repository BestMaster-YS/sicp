;; 集合可以含有重复元素

; 不变 O(n)
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else
          (element-of-set? x (cdr set)))))

; 无需检查 O(1)
(define (adjoin-set x set) (cons x set))

; remove-once
(define (remove-once v seq)
  (cond ((null? seq) '())
        ((equal? v (car seq))
         (cdr seq))
        (cons v (remove-once v (cdr seq)))))

; 交集 含重复重复元素集合的交集，这里选择实现交集还是有重复元素，但是必须两个集合都含有一定数量的该元素
; O(n^2)
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection (cdr set1) (remove-once (car set2) set2))))
        (else
          (intersection (cdr set1) set2))))

; 并集 可以含有重复元素则直接相并 O(n)
(define (union-set set1 set2) (append set1 set2))