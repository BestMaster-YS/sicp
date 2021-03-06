(load "util.scm")

;; 没有重复元素 O(n)
(define (adjoin-set x set)
  (if (null? set)
      (list x)
      (let ((fst (car set))
            (next (cdr set)))
        (cond ((= fst x) set)
              ((> fst x)
               (cons x set))
              ((< fst x)
               (cons fst (adjoin-set x next)))))))

(adjoin-set 50 '(1 2 4 45 54))
