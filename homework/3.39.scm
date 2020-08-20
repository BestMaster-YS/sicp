(define x 10)
(define s (make-serializer))

(parallel-execute
 (lambda () (set! x ((s (lambda () (* x x))))))
 (s (lambda () (set! x (+ x 1)))))

;; 顺序执行 101
;; 先执行第二个,再顺序执行第一个 121
;; 先执行第一个，但交错执行，由于串行化结果只有两个 101或121
