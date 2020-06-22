(load "util.scm")

; 只修改构造函数，不修改 deriv

;; 加数
(define (augend x)
  (if (= (length (cddr x)) 1)
      (caddr x)
      (append '(+) (cddr x)))
)

(define (make-sum a1 . a2)
  (cond ((= (length a2) 1)
         (let ((augend (car a2)))
           (cond ((and (number? a1) (number? augend)) (+ a1 augend))
                 ((=number? augend 0) a1)
                 ((=number? a1 0) augend)
                 (else
                  (list '+ a1 augend)))))
        ((=number? a1 0) (append '(+) a2))
        (else
          (append '(+ a1) a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (multiplicand p)
  (if (= (length (cddr p)) 1)
      (caddr p)
      (append '(*) (cddr p)))
)

(define (make-product m1 . m2)
  (cond ((= (length m2) 1)
         (let ((multiplicand (car m2)))
           (cond ((and (number? m1) (number? multiplicand)) (* m1 multiplicand))
                 ((or (=number? multiplicand 0) (=number? m1 0)) 0)
                 ((=number? m1 1) multiplicand)
                 ((=number? multiplicand 1) m1)
                 (else
                  (list '* m1 multiplicand)))))
        ((=number? m1 0) 0)
        ((=number? m1 1) (append (list '* ) m2))
        (else
          (append (list '* m1) m2))))


(deriv '(* x y (+ x 3)) 'x )
