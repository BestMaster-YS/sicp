
;; 与过滤器 not 类似
(define (negate operands frame-stream)
  (steam-flatmap
   (lambda (frame)
     (if (stream-null? (qeavl (negated-query operands)
                              (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))


(define (singleton-stream? stream)
  (and (not (stream-null? stream))
       (stream-null? (stream-cdr stream))))

;; 实现查询语言新的特殊形式 unique，当数据库中只有一个满足特殊查询的条目是成功

(define (uniquely-asserted pattern stream)
  (stream-flatmap
   (lambda (frame)
     (if (singleton-stream? (qeval (contents pattern)
                                   (singleton-stream frame)))
         stream
         the-empty-stream))
   frame-stream))

(put 'unique 'qeval uniquely-asserted)


