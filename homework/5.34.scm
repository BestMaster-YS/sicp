(load "compile.scm")
(load "io-lib.scm")

(define content
  (compile
   '(define (factorial n)
      (define (iter product counter)
        (if (> counter n)
            product
            (iter (* counter product)
                  (+ counter 1))))
      (iter 1 1))
   'val
   'next))

(write-file "5.34-compiled.scm" content)

;; diff https://www.diffchecker.com/J1eqxD21

;; 使用迭代会导致编译代码变长，编译 iter，执行 iter
;; 主要在于 diff 中的左边 38 和 右边 33 false-branch 后的行为
;; 这个 false-branch 是执行完 (= n 1) 和 (> counter n) 后的 false branch 
;; 迭代式编译后的代码为先保存 iter proc ，去计算 (* counter product) (+ counter 1)
;; 递归式编译后的代码为先保存 * proc，去计算 (factorial (- n 1))
;; 前者不需要保存 env proc continue,直接执行 compiled-procedure，后者需要保存 proc (*), env, 执行 compiled-procedure

