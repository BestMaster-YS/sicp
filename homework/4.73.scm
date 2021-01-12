;; flatten-stream 显式的使用 delay?

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

;; 执行 interleave 函数，两个参数会先进行求值，就导致 flattern-stream 无限调用，死循环
;; 必须显式的 delay 掉第二个参数

(define (flattern-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave
       (stream-car stream)
       (flattern-stream (stream-cdr stream)))))


