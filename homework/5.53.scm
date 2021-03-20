(load "compile.scm")
(load "io-lib.scm")

(define statement1
  '(define (factorial n)
     (if (= n 1)
         1
         (* (factorial (- n 1)) n))))

(define statement2
  '(define (factorial-alt n)
     (if (= n 1)
         1
         (* n (factorial-alt (- n 1))))))

(define content1
  (compile
   statement1
   'val
   'next))

(define content2
  (compile
   statement2
   'val
   'next))

(write-file "5.53-c1.scm" content1)
(write-file "5.53-c2.scm" content2)

;; 在两个文件中只有少量的汇编语句不同 ，其余则是 label 名不同


;; c1
;; 	(assign val (op lookup-variable-value) (const n) (reg env))
;;  (assign argl (op list) (reg val))
;;  (save argl)

;; c2
;; 	(save env)


;; c1
;; after-call9
;; (restore argl)

;; c2
;; after-call26
;; (assign argl (op list) (reg val))
;; (restore env)
;; (assign val (op lookup-variable-value) (const n) (reg env))


;; c1 保存 argl
;; c2 保存 env
;; 效率时相同的


