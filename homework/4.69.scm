(rule (grandson ?x ?y)
      (and (son ?x ?z)
           (son ?z ?y)))

(rule (end-in-grandson (grandson)))
(rule (end-in-grandson (?x . ?rest))
      (end-in-grandson ?rest))

;; great 关键词，向下再查询一次 son 关系
(rule ((grandson) ?x ?y) (grandson ?x ?y))
(rule ((great . ?rest) ?x ?y)
      ;; 先检查是否以 grandson 查询为结尾
      (and (end-in-grandson ?rest)
           (son ?x ?z)
           (?rest ?z ?y)))
