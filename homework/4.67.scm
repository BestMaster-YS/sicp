;; 安装循环监听器，首先维护一个栈，记录当前执行的规则，
;; 遇到规则内部的规则执行，分两种情况，若是执行栈中没有则加入
;; 若是执行栈中有则说明当前规则会陷入死循环。
;; 数据结构也可以换成链表
