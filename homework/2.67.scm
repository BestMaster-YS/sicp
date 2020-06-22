(load "util.scm")

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree 
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-meesage '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-meesage sample-tree)
