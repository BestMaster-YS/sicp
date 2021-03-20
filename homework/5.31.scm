
;; env,argl,proc相关全部的 save 和 restore 都是多余的
(f 'x 'y)

;; 同上, 在求值运算符 (f) 时，跳转到 ev-application ，但是也不涉及任何寄存器的改变
((f) 'x 'y)

;; proc 和 argl 在求值 (g 'x) 之前要 save，求值完成后 restore，虽然 y 时变量但是不影响
;; 整个过程的寄存器变化
(f (g 'x) y)

;; proc 和 argl 在求值 (g 'x) 之前要 save，求值完成后 restore
(f (g 'x) 'y)




