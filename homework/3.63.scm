(load "util.scm")

(define (sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream x))))

;; 这里 sqrt-stream 是一个函数，当它调用时并没有应用 memo-proc 的效果，调用 (sqrt-stream 1) 返回 1，调用 (sqrt-stream 2) 还是从 (cons-stream 1.0);;开始执行，到了内部 sqrt-stream x时，才开始展开计算






