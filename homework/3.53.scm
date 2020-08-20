;;

(define s (cons-stream 1 (add-streams s s)))

;; 结果： 1 2 4 .. 2^(- n 1)
;; s 计算下个值时会进行分裂，1 -> 2 -> 4 -> ...

