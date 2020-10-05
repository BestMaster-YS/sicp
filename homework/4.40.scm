(load "util.scm")

;; 初始化时可以缩小范围
;
;(require (not (= baker 5)))
;(require (not (= cooper 1)))
;(require (not (= flectcher 5)))
;(require (not (= flectcher 1)))
;miller > cooper

;; 可以不用一开始初始化全部人的位置

(define (multiple-dwelling)
  (let ((cooper (amb 2 3 4))
        (miller (amb 3 4 5)))
    (let ((flectcher 2 3 4))
      (requrie (not (= (abs (- cooper flectcher)))))
      (let ((smith (amb 1 2 3 4 5)))
        (require (not (= (abs (- smith flectcher)))))
        (let ((baker (amb 1 2 3 4)))
          (require (distinct? cooper miller smith flectcher baker))
          (list (list 'baker baker)
                (list 'smith smith)
                (list 'miller miller)
                (list 'flectcher flectcher)
                (list 'cooper cooper))))))







