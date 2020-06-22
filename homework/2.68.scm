(load "util.scm")

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree 
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-meesage '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;; 将字符信息编码为0/1表
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (contain? symbol set)
  (let ((m (memq symbol set)))
    (if (pair? m)
        #t
        #f)))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      '()
      (let ((left (left-branch tree))
            (right (right-branch tree)))
        (let ((left-symbols (symbols left))
              (right-symbols (symbols right)))
          (cond ((contain? symbol left-symbols) (cons 0 (encode-symbol symbol left)))
                ((contain? symbol right-symbols) (cons 1 (encode-symbol symbol right)))
                (else
                  (error "not found symbol in tree -- ENCODE-SYMBOL" symbol)))))))

(define test '(A D A B B C A))

(encode test sample-tree)
(decode (encode test sample-tree) sample-tree)