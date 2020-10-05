;; 增加麦斯尔和弗莱舍不住相邻楼层的条件，不是已经在条件中了吗？

(load "util.scm")

;; 逻辑谜题

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (flectcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper flectcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= flectcher 5)))
    (require (not (= flectcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith flectcher)) 1)))
    (require (not (= (abs (- flectcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'flectcher flectcher)
          (list 'miller miller)
          (list 'smith smith))))





