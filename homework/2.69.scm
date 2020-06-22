(load "util.scm")

;; 生成Huffman编码树

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;; 将有序对偶表转化为Huffman树
(define (successive-merge set)
  (if (= (length set) 2)
      (make-code-tree (cadr set) (car set))
      (let ((min1 (car set))
            (min2 (cadr set))
            (next (cddr set)))
        (let ((new (make-code-tree min2 min1)))
          (successive-merge (adjoin-set new next))))))

(define test '((A 4) (B 2) (D 1) (C 1)))

(define target '(A B A A B D A C))

(generate-huffman-tree test)

(encode target (generate-huffman-tree test))

(decode (encode target (generate-huffman-tree test)) (generate-huffman-tree test))

