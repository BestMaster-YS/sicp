;; and 查询低效
;; 数据库的数据为 N，一次典型查询产生的输出框架个数等比于 N， N/k

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunctions? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))

;; 兼容两个流



