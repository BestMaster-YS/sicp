(load "util.scm")

(define (sum-odd-square tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else
          (+ (sum-odd-square (car tree))
             (sum-off-square (cdr tree)))))
)

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        '()
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1)))))
  )
  (next 0)
)

(even-fibs 100)

(define nil '())

;; 序列操作 信号流操作

;; 映射
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items))))
)

; 过滤
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else
         (filter predicate (cdr sequence))))
)

(filter odd? (list 1 2 3 4 5)) 

; 积累

(define (accmulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accmulate op initial (cdr sequence))))
)

(accmulate + 0 (list 1 2 3 4))

;; 枚举区间
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high)))
)

(enumerate-interval 2 7)

; 枚举树的叶子节点

(define (enumerate-trees tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-trees (car tree))
                      (enumerate-trees (cdr tree)))))
)

(enumerate-trees (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (square x) (* x x))

(define (sum-odd-square tree)
  (accmulate +
             0
             (map square
                  (filter odd? (enumerate-trees tree))))
)

(sum-odd-square (list 1 (list 2 (list 3 4) 5) (list 6 7)))


(define (list-fib-squares n)
  (accmulate cons
             nil
             (map square (map fib (enumerate-interval 0 n))))
)

(list-fib-squares 5)

(define (product-of-squares-odd-of-elements sequence)
  (accmulate *
             1
             (map square
                  (filter odd? (enumerate-trees sequence))))
)

;; 嵌套映射

;; 将序列元素映射序列
(define (flatmap proc seq)
  (accmulate append nil (map proc seq)))


(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pair n)
  (map make-pair-sum
       (filter prime-sum?
	       (flatmap
          (lambda (i)
            (map (lambda (j) (list i j))
                (enumerate-interval 1 (- i 1))))
          (enumerate-interval 1 n)))))
(prime-sum-pair 10)

;; 嵌套映射生成全排列

(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
		 (map (lambda (p) (cons x p))
		      (permutations (remove x s))))
	       s)))

(define (remove v sequence)
  (filter (lambda (s) (not (= s v)))
	  sequence))


(permutations (list 1 2 3))


