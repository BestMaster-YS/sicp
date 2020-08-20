(load "util.scm")

;; pairs 中interleave的第二参数会无限递归执行
(define (pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs (stream-cdr s) (stream-cdr t))))

(take 10 (pairs integers integers))

