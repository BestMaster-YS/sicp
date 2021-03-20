(load "compile.scm")
(load "io-lib.scm")

;; 去掉 preserving 的寄存器操作
(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        ;; 去掉判断
        ;; (if (and (needs-register? seq2 first-reg)
           ;;      (modifies-register? seq1 first-reg))
        (preserving (cdr regs)
                    (make-instruction-sequence
                     (list-union (list first-reg)
                                 (registers-needed seq1))
                     (list-difference (registers-modified seq1)
                                      (list first-reg))
                     (append `((save ,first-reg))
                             (statements seq1)
                             `((restore ,first-reg))))
                    seq2)
            ;;(preserving (cdr regs) seq1 seq2))
        )))


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

(write-file "5.37-c1.scm" content1)

;; 字符长度
;; 5.33-c1:2355
;; 5.37-c1:3272







