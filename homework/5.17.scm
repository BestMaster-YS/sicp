;; 改变指令结构为三元组，新增标号字段，(id, inst-content, action)
;; 对每个指令进行标号

;; 1 (test (op =) (reg b) (const 0))
;; 2 (branch (label gcd-done))
;; 3 (assign t (op rem) (reg a) (reg b))
;; 4 (assign a (reg b))


