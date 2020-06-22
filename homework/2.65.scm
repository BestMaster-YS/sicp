(load "util.scm")

;; 先转化为 list 再转化为 tree

(define (union-tree tree1 tree2)
  (list->tree
    (union-set (tree->list tree1)
               (tree->list tree2))))

(define (intersection-tree tree1 tree2)
  (list->tree
    (intersection-set (tree->list tree1)
                      (tree->list tree2))))

