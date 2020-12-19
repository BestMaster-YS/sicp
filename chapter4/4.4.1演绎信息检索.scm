
(address (Bitdiddle ben) (Slumerville (Ridge Road) 10))
(job (Bitdiddle ben) (computer wizard))
(salary (Bitdiddle ben) 60000)

(address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
(job (Hacker Alyssa P) (computer programmer))
(salary (Hacker Alyssa P) 40000)
(supervisor (Hacker Alyssa P) (Bitdiddle Ben))

(address (Fect Cy D) (Cambridge (Ames Street) 3))
(job (Fect Cy D) (computer programmer))
(salary (Fect Cy D) 35000)
(supervisor (Fect Cy D) (Bitdiddle Ben))

(address (Tweakit Lem E) (Boston (Bay State Road) 22))
(job (Tweakit Lem E) (computer technician))
(salary (Tweakit Lem E) 25000)
(supervisor (Tweakit Lem E) (Bitdiddle Ben))

(address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
(job (Reasoner Louis) (computer programmer trainee))
(salary (Reasoner Louis) 30000)
(supervisor (Reasoner Louis) (Hacker Alyssa P))

(supervisor (Bitdiddle Ben) (Warbucks Oliver))
(address (Warbucks Oliver) (Swellesley (Top Heap Road)))
(job (Warbucks Oliver) (administration big wheel))
(salary (Warbucks Oliver) 150000)

(address (Scrooge Eben) (Weston (Shady Lane) 10))
(job (Scrooge Eben) (accounting chief accountant))
(salary (Scrooge Eben) 75000)
(supervisor (Scrooge Eben) (Warbucks Oliver))

(address (Cratchet Robert) (Allston (N Harvard Street) 16))
(job (Cratchet Robert) (accounting scrivener))
(salary (Cratchet Robert) 18000)
(supervisor (Cratchet Robert) (Scrooge Eben))

(address (Aull DeWitt) (Slumerville (Onion Square) 5))
(job (Aull DeWitt) (administration secretary))
(salary (Aull DeWitt) 25000)
(supervisor (Aull DeWitt) (Warbucks Oliver))

(can-do-job (computer wizard) (computer programmer))
(can-do-job (computer wizard) (computer technician))

(can-do-job (computer programmer) (computer programmer trainee))
(can-do-job (administration secretary) (administration big wheel))

;; 简单查询 匹配 (job () (computer programmer))
(job ?x (computer programmer))

;; 查询所有员工的地址
(address ?x ?y)

;; 同一查询变量，x = supervisor x
(supervisor ?x ?x)

;; 查询
(job ?x (computer ?type))
;; (job xxx (computer x y)) 则不匹配

;; 查询所有的job为 computer 类下的员工，对于 (computer x) (compter x y) 都匹配
(job ?x (computer . ?type))

;; 对于查询语言的处理描述如下
;; * 系统将找出使得查询模式中变量满足这一模式的所有赋值，为这些变量找出所有值的集合
;; * 系统对查询的响应方式，就是列出查询模式的所有满足要求的实例

;; 复合查询，and,or,not 逻辑操作

(and (job ?person (computer programmer))
     (address ?person ?where))

(or (supervisor ?x (Bitdiddle ben))
    (supervisor ? (Hacker Alyssa P)))

(and (supervisor ?person (Bitdiddle ben))
     (not (job ?x (computer programmer))))

;; list-value 组合方法，当 list-value 被用作模式的第一个元素时，就说明下一个元素是一个lisp的谓词，应将它应用于作为其参数的其余元素。
;; 查询出工资高于30000的人
(and (salary ?person ?amount)
     (list-value > ?amount 30000))


;; 规则

(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))))

(rule (same ?person ?person))

(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))

(lives-near ?x (Bitdiddle ben))

(and (job ?x (computer programmer))
     (lives-near ?x (Bitdiddle ben)))

;; 规则里可以使用其他规则，也可以递归
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))


;; 将逻辑看做程序，对于 append 操作可以看成为两个规则

(apend-to-form x y z)

(rule (append-to-form () ?y ?y))

(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))

;; query input
(append-to-form (a b) (c d) ?z)
;; query output
(append-to-form (a b) (c d) (a b c d))

(append-to-form (a b) ?y (a b c))
(append-to-form (a b) (c) (a b c))


(append-to-form ?x ?y (a b c d))

(append-to-form (a b c d) () (a b c d))
(append-to-form (a b c) (d) (a b c d))
(append-to-form (a b) (c d) (a b c d))
(append-to-form (a) (b c d) (a b c d))
(append-to-form () (a b c d) (a b c d))
















