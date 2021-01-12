

;; 在 negate , lisp-value 和 find-assertions 中采用更简单的 stream-flatmap 版本

(define (simple-stream-flatmap proc s)
  (simple-fattern (stream-map proc s)))

;; 因为 Alyssa P.Hacker 提到被映射到框架流的过程总是产生一个空流或者单元素流。
(define (simple-fattern stream)
  (stream-map stream-car
              (stream-filter (lambda (s)
                               (not (stream-null? s)))
                             stream)))


;; 不会改变






