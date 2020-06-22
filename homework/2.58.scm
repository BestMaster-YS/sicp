(load "util.scm")

;; 只修改构造、选择函数和谓词
;; (a) 所有复合表达式都加上括号，且 + 和 * 只接受两个参数

; + 
(define (sum? exp) (and (pair? exp) (eq? (cadr exp) '+ )))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else
          (list a1 '+ a2)))
)

;; 被加数
(define (addend x) (car x))

;; 加数
(define (augend x) (caddr x))

; *
(define (product? x) (and (pair? x) (eq? (cadr x) '* )))

(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else
          (list m1 '* m2))))


(deriv '(x + (3 * (x + (y + 2)))) 'x )
(deriv (deriv '(x + (x * x)) 'x ) 'x )


;; (b) 支持多参数

; + 
; 第一层级有 + 
(define (sum? exp)
  (define (iter rest)
    (if (null? rest)
        #f
        (let ((cur (car rest))
              (next (cdr rest)))
          (cond ((pair? cur) (iter next))
                ((eq? cur '+ ) #t )
                (else (iter next))))))
  (and (pair? exp)
       (iter exp)))

;; TODO 能否实现原生 apply 函数
(define (make-sum a1 . a2)
  (cond ((null? a2) a1)
        ((= (length a2) 1)
         (let ((augend (car a2)))
           (cond ((and (number? a1) (number? augend)) (+ a1 augend))
                 ((=number? augend 0) a1)
                 ((=number? a1 0) augend)
                 (else
                  (list a1 '+ augend)))))
        ((=number? a1 0) (apply make-sum a2))
        (else
          (append (list a1 '+ ) (apply make-sum a2)))))

(define (separate-exp v exp)
  (define (iter addend augend)
    (if (eq? (car augend) v )
        (cons addend (cdr augend))
        (iter (append addend (list (car augend)))
              (cdr augend))))
  (iter '() exp))


;; 被加数
(define (addend exp)
  (let ((v (car (separate-exp '+ exp))))
    (if (= (length v) 1)
        (car v)
        v)))

;; 加数
(define (augend exp)
  (let ((v (cdr (separate-exp '+ exp))))
    (if (= (length v) 1)
        (car v)
        v)))

; * 第一层级不能存在 + ,只能存在 *
(define (product? exp)
  (define (iter exist rest)
    (if (null? rest)
        exist
        (let ((cur (car rest))
              (next (cdr rest)))
          (cond ((pair? cur) (iter exist next))
                ((eq? cur '+ ) #f )
                ((eq? cur '* ) (iter #t next))
                (else (iter exist next))))))
  (and (pair? exp)
       (iter #f exp)))


(define (multiplier exp)
  (let ((v (car (separate-exp '* exp))))
    (if (= (length v) 1)
        (car v)
        v)))

(define (multiplicand exp)
  (let ((v (cdr (separate-exp '* exp))))
    (if (= (length v) 1)
        (car v)
        v)))

(define (make-product m1 . m2)
  (cond ((null? m2)
         m1)
        ((= (length m2) 1)
         (let ((multiplicand (car m2)))
           (cond ((and (number? m1) (number? multiplicand)) (* m1 multiplicand))
                 ((or (=number? multiplicand 0) (=number? m1 0)) 0)
                 ((=number? m1 1) multiplicand)
                 ((=number? multiplicand 1) m1)
                 (else
                  (list m1 '* multiplicand)))))
        ((=number? m1 0) 0)
        ((=number? m1 1) (apply make-product m2))
        (else
          (append (list m1 '* ) (apply make-product m2)))))


(deriv '(x + 3 * (x + y + 2)) 'x )

;(separate-exp '* '(x + x * 3) )