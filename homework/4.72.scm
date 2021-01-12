;; 为什么 disjoin 和 stream-flatmap 以交错方式合并流，而不是简单地连接？

;; 因为查询时第一个条件可能不存在可满足的断言
;; 若是简单的相连就会导致无法查询到满足第二个条件的数据
(rule (test ?a ?b)
      (or (married ?a Lucy)
          (married ?b Bobo)))

(define (disjoin disjuncts frame-stream)
  (if (empty-didjunctions? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))

(define (stream-flatmap proc s)
  (flattern-stream (stream-map proc s)))

(define (flattern-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flattern-stream (stream-cdr stream))))))













