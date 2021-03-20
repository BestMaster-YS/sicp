(load "compile.scm")
(load "io-lib.scm")

;; 现在编译器对于 primitive-application 也会进行不必要的检查测试，增加编译后的指令长度。

;; a) 新增 arg1 和 arg2 寄存器

(define (spread-argument operands)
  (let ((first-operand (compile (car operands) 'arg1 'next))
        (second-operand (compile (cadr operands) 'arg2 'next)))
     (preserving
      '(arg1)
      first-operand
      ;; 求值第二个参数时需要保护 arg1
      second-operand)))


;; b)
;; 将 (+ x 1) 编译为以下指令序列
;; (assgin arg1 (op lookup-variable-value) (const x) (reg env))
;; (assgin arg2 (const 1))
;; (assgin val (op +) (reg arg1) (reg arg2))

(define (primitive-application? exp)
  (memq (car exp) '(- =)))

(define (compile-primitive-application exp target linkage)
  (let ((operand-codes
         (spread-argument (operands exp)))
        (primitive-operator (operator exp)))
    (end-with-linkage linkage
                      (append-instruction-sequences
                       operand-codes
                       (make-instruction-sequence '(arg1 arg2) (list target)
                                                   `((assign ,target (op ,primitive-operator) (reg arg1) (reg arg2))))))))

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
        ((primitive-application? exp)
         (compile-primitive-application exp target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

;; c)

(define statement1
  '(define (factorial n)
     (if (= n 1)
         1
         (* (factorial (- n 1)) n))))

(define content1
  (compile
   statement1
   'val
   'next))

;; (write-file "5.38-c1.scm" content1)

;; https://www.diffchecker.com/CbQrQYUk
;; 优化前 84 行，优化后 43 行

;; d)
;; 对 compile-primitive-application 优化，可以处理任意参数
;; 思路对参数进行递归的计算，如 (+ 1 2 3) 可以编译为以下指令序列
;; (assign arg1 (const 1))
;; (assign arg2 (const 2))
;; (assign arg1 (op +) (reg arg1) (reg arg2))
;; (assign arg2 (const 3))
;; (assign val  (op +) (reg arg1) (reg arg2))

(define (primitive-application-arbitrary? exp)
  (memq (car exp) '(+ *)))

(define (operand2 operands)
  (list (car operands) (cadr operands)))

(define (rest2-operand operands)
  (cddr operands))

(define (compile-primitive-arbitrary-application exp target linkage)
  (let ((primitive-operator (operator exp)))
    (if (< (length (operands exp)) 2)
        (error "ARGUMENTS ERROR, TO LESS" exp)
        (end-with-linkage linkage
                          (append-instruction-sequences
                           (compile-operands (operands exp) primitive-operator (empty-instruction-sequence) #f)
                           (make-instruction-sequence '(arg1 arg2) (list target)
                                                      `((assign ,target (op ,primitive-operator) (reg arg1) (reg arg2)))))))))

(define (compile-operands operands primitive-operator res-sequences arg1-has-value?)
  (let ((len (length operands)))
    (cond ((= len 1)
           ;; 说明前面的计算结果都在 arg1 中
           (let ((arg2-code
                  (compile (car operands) 'arg2 'next)))
             (preserving
              '(arg1)
              res-sequences
              arg2-code)))
          ((= len 0)
           res-sequences)
          ;; 剩余参数大于2
          (else
           (let ((compile-2-operands
                  (spread-argument (operand2 operands))))
             (if arg1-has-value?
                 ;; 当 arg1 有值时，需要将两个参数计算的值赋给 arg2，再将 arg1 和 arg2 计算的值赋给 arg1
                 (compile-operands
                  (rest2-operand operands)
                  primitive-operator
                  (append-instruction-sequences res-sequences
                                                (preserving
                                                 '(arg1)
                                                 (append-instruction-sequences
                                                  compile-2-operands
                                                  (make-instruction-sequence '(arg1 arg2) '(arg2)
                                                                             `((assign arg2 (op ,primitive-operator) (reg arg1) (reg arg2)))))
                                                 (make-instruction-sequence '(arg1 arg2) '(arg1)
                                                                            `((assign arg1 (op ,primitive-operator) (reg arg1) (reg aeg2))))))
                  #t)
                 ;; 当 arg1 没有值时，需要将两个参数计算的值赋给 arg1，然后进入递归
                 (compile-operands
                  (rest2-operand operands)
                  primitive-operator
                  (append-instruction-sequences res-sequences
                                                (append-instruction-sequences
                                                 compile-2-operands
                                                 (make-instruction-sequence '(arg1 arg2) '(arg1)
                                                                            `((assign arg1 (op ,primitive-operator) (reg arg1) (reg arg2))))))
                  #t)))))))


(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
        ((primitive-application? exp)
         (compile-primitive-application exp target linkage))
        ((primitive-application-arbitrary? exp)
         (compile-primitive-arbitrary-application exp target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define statement2
  '(define (plus3 x y)
     (+ 3 x y)))

(define content2
  (compile
   statement2
   'val
   'next))

(write-file "5.38-c2.scm" content2)

