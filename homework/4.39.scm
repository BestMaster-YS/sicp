(load "util.scm")

;; 不影响答案，但影响运行效率
;; 约束范围大的条件应该在约束范围较小的条件前面

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (flectcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper flectcher miller smith)))
    (require (> miller cooper))
    (require (not (= (abs (- smith flectcher)) 1)))
    (require (not (= (abs (- flectcher cooper)) 1)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= flectcher 5)))
    (require (not (= flectcher 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'flectcher flectcher)
          (list 'miller miller)
          (list 'smith smith))))


