;; 实现 last-pair


;; ?u 可以表示为一个元素或者一个表

;; 只有一个元素的表时
(rule (last-pair (?u) ?u))

;; 存在大于两个元素的表时

(rule (last-pair (?v . ?z) ?x)
      (last-pair ?z ?x))



