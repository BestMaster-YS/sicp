(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else
          (element-of-set? x (cdr set)))))

;; 表元素未重复
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection (cdr set1) set2)))
        (else
          (intersection (cdr set1) set2))))

;; 集合作为排序的表
; 0(n)
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else
          (element-of-set? x (cdr set)))))

; O(n)
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (next1 (cdr set1))
            (x2 (car set2)) (next2 (cdr set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set next1 next2)))
              ((< x1 x2)
               (intersection-set next1 set2))
              ((< x2 x1)
               (intersection-set set1 next2))))))

; 集合作为二叉树 用表来表示树，第一项为数据，第二、三项为左右子树

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right) (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((> x (entry set))
         (element-of-set? x (right-branch set)))
        ((< x (entry set))
         (element-of-set? x (left-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((> x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((< x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;; 集合与信息检索

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

