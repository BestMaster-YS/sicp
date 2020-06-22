; 创建叶子节点
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf ))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
;; 创建树
(define (make-code-tree left right)
  (list left right (append (symbols left) (symbols right))
                   (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

;; 判断是树还是叶子节点
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; 解码过程 接受0、1表和一棵Huffman树，将0/1表解码为字符
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
                (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                     ; 解码下一个字符
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (dncode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 1) (right-branch branch))
        ((= bit 0) (left-branch branch))
        (else
          (error "bad bit -- CHOOSE BRANCH" bit))))


;; 带权重元素的集合
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

;; 将符号-权重对偶的表排序
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))


