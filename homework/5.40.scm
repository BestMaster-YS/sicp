(load "scm-machine.scm")
(load "lexical.scm")
(load "io-lib.scm")


(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

;;;SECTION 5.5.2

;;;linkage code

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue) '()
          '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence '() '()
          `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
   instruction-sequence
   (compile-linkage linkage)))


;;;simple expressions

(define (compile-self-evaluating exp target linkage compile-time-environment)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage compile-time-environment)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    `((assign ,target (const ,(text-of-quotation exp)))))))



;; lexical-address 词法地址的作用在于在编译期间找到对应变量的地址
;; 在执行期间能快速从 lexical-address-lookup 来找到环境对应的变量

(define (compile-variable exp target linkage compile-time-environment)
  (let ((lexical-addr (find-variable exp compile-time-environment)))
    (end-with-linkage linkage
                      (make-instruction-sequence '(env) (list target)
                                                 (if (eq? lexical-addr 'not-found)
                                                     `((assign ,target
                                                               (op lookup-variable-value)
                                                               (const ,exp)
                                                               (reg env)))
                                                     `((assign ,target
                                                               (op lexical-address-lookup)
                                                               (const ,lexical-addr)
                                                               (reg env))))))))

(define (compile-assignment exp target linkage compile-time-environment)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next compile-time-environment)))
    (let ((lexical-addr (find-variable var compile-time-environment)))
      (end-with-linkage linkage
                        (preserving '(env)
                                    get-value-code
                                    (if (eq? lexical-addr 'not-found)
                                        (make-instruction-sequence '(env val) (list target)
                                                                   `((perform (op set-variable-value!)
                                                                              (const ,var)
                                                                              (reg val)
                                                                              (reg env))
                                                                     (assign ,target (const ok))))
                                        (make-instruction-sequence '(env val) (list target)
                                                                   `((perform (op lexical-address-set!)
                                                                              (const ,lexical-addr)
                                                                              (reg env))
                                                                     (assign ,target (const ok))))))))))

(define (compile-definition exp target linkage compile-time-environment)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp) 'val 'next compile-time-environment)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
       `((perform (op define-variable!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))


;;;conditional expressions

;;;labels (from footnote)
(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string->symbol
    (string-append (symbol->string name)
                   (number->string (new-label-number)))))
;; end of footnote

(define (compile-if exp target linkage compile-time-environment)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next compile-time-environment))
            (c-code
             (compile
              (if-consequent exp) target consequent-linkage compile-time-environment))
            (a-code
             (compile (if-alternative exp) target linkage compile-time-environment)))
        (preserving '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence '(val) '()
           `((test (op false?) (reg val))
             (branch (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences t-branch c-code)
           (append-instruction-sequences f-branch a-code))
          after-if))))))

;;; sequences

(define (compile-sequence seq target linkage compile-time-environment)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage compile-time-environment)
      (preserving '(env continue)
       (compile (first-exp seq) target 'next compile-time-environment)
       (compile-sequence (rest-exps seq) target linkage compile-time-environment))))

;;;lambda expressions

(define (compile-lambda exp target linkage compile-time-environment)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
         (make-instruction-sequence '(env) (list target)
          `((assign ,target
                    (op make-compiled-procedure)
                    (label ,proc-entry)
                    (reg env)))))
        (compile-lambda-body exp proc-entry compile-time-environment))
       after-lambda))))

(define (compile-lambda-body exp proc-entry compile-time-environment)
  (let ((formals (lambda-parameters exp)))
    ;; 扩展 compile-time-environment
    (let ((new-compile-time-environment (cons formals compile-time-environment)))
      (append-instruction-sequences
       (make-instruction-sequence '(env proc argl) '(env)
                                  `(,proc-entry
                                    (assign env (op compiled-procedure-env) (reg proc))
                                    (assign env
                                            (op extend-environment)
                                            (const ,formals)
                                            (reg argl)
                                            (reg env))))
       (compile-sequence (lambda-body exp) 'val 'return new-compile-time-environment)))))

;;;SECTION 5.5.3

;;;combinations

(define (compile-application exp target linkage compile-time-environment)
  (let ((proc-code (compile (operator exp) 'proc 'next compile-time-environment))
        (operand-codes
         (map (lambda (operand) (compile operand 'val 'next compile-time-environment))
              (operands exp))))
    (preserving '(env continue)
     proc-code
     (preserving '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                 '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
               code-to-get-last-arg
               (code-to-get-rest-args
                (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
          (car operand-codes)
          (make-instruction-sequence '(val argl) '(argl)
           '((assign argl
              (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-codes))))))

;;;applying procedures

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage linkage
          (make-instruction-sequence '(proc argl)
                                     (list target)
           `((assign ,target
                     (op apply-primitive-procedure)
                     (reg proc)
                     (reg argl)))))))
       after-call))))

;;;applying compiled procedures

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
           `((assign continue (label ,linkage))
             (assign val (op compiled-procedure-entry)
                         (reg proc))
             (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
            `((assign continue (label ,proc-return))
              (assign val (op compiled-procedure-entry)
                          (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
          '((assign val (op compiled-procedure-entry)
                        (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target))))

;; footnote
(define all-regs '(env proc val argl continue))


;;;SECTION 5.5.4

(define (registers-needed s)
  (if (symbol? s) '() (car s)))

(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))

(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))

(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))


(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
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
            (preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq) (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1) (statements seq2))))

;; 5.38 练习新增

(define (spread-argument operands compile-time-environment)
  (let ((first-operand (compile (car operands) 'arg1 'next compile-time-environment))
        (second-operand (compile (cadr operands) 'arg2 'next compile-time-environment)))
     (preserving
      '(arg1)
      first-operand
      second-operand)))

(define (primitive-application? exp)
  (memq (car exp) '(- =)))

(define (compile-primitive-application exp target linkage compile-time-environment)
  (let ((operand-codes
         (spread-argument (operands exp) compile-time-environment))
        (primitive-operator (operator exp)))
    (end-with-linkage linkage
                      (append-instruction-sequences
                       operand-codes
                       (make-instruction-sequence '(arg1 arg2) (list target)
                                                   `((assign ,target (op ,primitive-operator) (reg arg1) (reg arg2))))))))

(define (primitive-application-arbitrary? exp)
  (memq (car exp) '(+ *)))

(define (operand2 operands)
  (list (car operands) (cadr operands)))

(define (rest2-operand operands)
  (cddr operands))

(define (compile-primitive-arbitrary-application exp target linkage compile-time-environment)
  (let ((primitive-operator (operator exp)))
    (if (< (length (operands exp)) 2)
        (error "ARGUMENTS ERROR, TO LESS" exp)
        (end-with-linkage linkage
                          (append-instruction-sequences
                           (compile-operands (operands exp) primitive-operator (empty-instruction-sequence) #f compile-time-environment)
                           (make-instruction-sequence '(arg1 arg2) (list target)
                                                      `((assign ,target (op ,primitive-operator) (reg arg1) (reg arg2)))))))))

(define (compile-operands operands primitive-operator res-sequences arg1-has-value? compile-time-environment)
  (let ((len (length operands)))
    (cond ((= len 1)
           (let ((arg2-code
                  (compile (car operands) 'arg2 'next compile-time-environment)))
             (preserving
              '(arg1)
              res-sequences
              arg2-code)))
          ((= len 0)
           res-sequences)
          (else
           (let ((compile-2-operands
                  (spread-argument (operand2 operands) compile-time-environment)))
             (if arg1-has-value?
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
                  #t
                  compile-time-environment)
                 (compile-operands
                  (rest2-operand operands)
                  primitive-operator
                  (append-instruction-sequences res-sequences
                                                (append-instruction-sequences
                                                 compile-2-operands
                                                 (make-instruction-sequence '(arg1 arg2) '(arg1)
                                                                            `((assign arg1 (op ,primitive-operator) (reg arg1) (reg arg2))))))
                  #t
                  compile-time-environment)))))))

(define (compile exp target linkage compile-time-environment)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage compile-time-environment))
        ((quoted? exp) (compile-quoted exp target linkage compile-time-environment))
        ((variable? exp)
         (compile-variable exp target linkage compile-time-environment))
        ((assignment? exp)
         (compile-assignment exp target linkage compile-time-environment))
        ((definition? exp)
         (compile-definition exp target linkage compile-time-environment))
        ((if? exp) (compile-if exp target linkage compile-time-environment))
        ((lambda? exp) (compile-lambda exp target linkage compile-time-environment))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage
                           compile-time-environment))
        ((cond? exp) (compile (cond->if exp) target linkage compile-time-environment))
        ((primitive-application? exp)
         (compile-primitive-application exp target linkage compile-time-environment))
        ((primitive-application-arbitrary? exp)
         (compile-primitive-arbitrary-application exp target linkage compile-time-environment))
        ((application? exp)
         (compile-application exp target linkage compile-time-environment))
        (else
         (error "Unknown expression type -- COMPILE" exp))))



'(COMPILER LOADED)







