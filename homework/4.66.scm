(sum ?amount
     (and (job ?x (compuetr programmer))
          (salary ?x ?amount)))

(sum ?amount
     (and (wheel who?)
          (salary who? ?amount)))


;; 当查询符合 wheel 规则的人的薪资总和时， 重复计算了4次 Oliver Warbucks 的薪资
;; 在执行 sum 之前，对 ?amount 变量有关的数据断言进行去重，这里可以按照 who? 进行去重。
