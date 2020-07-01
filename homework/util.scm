;; 定义求平方根
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (square x)
  (* x x))

;; 平均值
(define (average x y)
  (/ (+ x y) 2))

;; 进一步提高准确性
(define (improve guess x)
  (average guess (/ x guess)))

; (define (good-enough? guess x)
;   (< (abs (- (square guess) x)) 0.001))
;; 改进版本
(define (good-enough? guess x)
  (< (/ (abs (- guess (improve guess x))) guess)
     0.001))

(define (even? n)
  (= (remainder n 2) 0)
)

(define (odd? n)
  (= (remainder n 2) 1)
)

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (cube x) (* x x x))

(define (double a) (+ a a))
(define (halve a) (/ a 2))

;; 求幂
(define (fast-expt b n)
	(fast-expt-iter b n 1)  
)
;; 迭代方法计算幂
(define (fast-expt-iter b n a)
	(cond ((= n 0) a)
        ((even? n) (fast-expt-iter (square b)
                                   (/ n 2)
                                   a))
        (else (fast-expt-iter b (- n 1) (* a b))))
)

;; GCD
(define (gcd a b)
	(if (= b 0)
      a
      (gcd b (remainder a b)))
)

;; 素数检测
(define (smallest-divisor n)
	  (find-divisor n 2)
)

(define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1))))  
)

(define (divides? a b)
	(= 0 (remainder b a))
)

(define (prime? n)
	(= (smallest-divisor n) n)
)

;; 费马检测

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a)
  )
  (try-it (+ 1 (random (- n 1))))
)

(define (fast-prime? n times)
	(cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false))
)

;; 1.3 高阶函数作为抽象

(define (inc a) (+ a 1))

(define (identity a) a)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b)))
)

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b)))
)

(define (filter-accumulate filter combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((filter a) (combiner (term a) (filter-accumulate filter
                                                          combiner
                                                          null-value
                                                          term
                                                          (next a)
                                                          next
                                                          b)))
        (else (filter-accumulate filter combiner null-value term (next a) next b)))
)

; 不动点函数
(define tolerance 0.00001)

(define (fix-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance)
  )

  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next)))
  )
  (try first-guess)
)

;; 连分式
(define (cont-frac n d k)
  (define (iter i prev)
    (if (= i 1)
        (/ (n i) (+ (d i) prev))
        (iter (- i 1) (/ (n i) (+ (d i) prev))))
  )
  (iter (- k 1) (/ (n k) (d k)))
)

;; 平均阻尼
(define (average-damp f)
  (lambda (x) (average x (f x)))
)

;; 牛顿法
(define dx 0.00001)

(define (deriv g)
	(lambda (x) (/ (- (g (+ x dx))
                    (g x))
                 dx))
)

(define (newton-transform g)
	(lambda (x) (- x
                 (/ (g x)
                    ((deriv g) x))))  
)

(define (newton-method g guess)
	(fix-point (newton-transform g) guess)  
)

; 抽象和第一级过程

(define (fixed-point-of-transform g transform guess)
  (fix-point (transform g) guess)
)

(define (double-call f)
  (lambda (x) (f (f x)))
)

;; compose
(define (compose f g)
  (lambda (x) (f (g x)))
)

;;
(define (repeat f n)
  (if (= n 1)
      (lambda (x) (f x))
      (compose (repeat f (- n 1)) f))
)

;;
;; 求幂 fast-expt 需处理 n = 0 的情形
(define (expt base n)
  (if (= n 0)
      1
      (fast-expt base n))
)

;; 平均阻尼 repeat
(define (repeat-average-damp n)
  (repeat average-damp n)
)

(define (damped-nth-root n times-damp)
  (lambda (x)
          (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
                                    (repeat-average-damp times-damp)
                                    1.0))
)

(define (lg n)
  (cond ((> (/ n 2) 1)
         (+ (lg (/ n 2)) 1))
        ((< (/ n 2) 1)
         0)
        (else 1))
)

(define (nth-root n)
  (damped-nth-root n (lg n))
)


;; 有理数构造
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g)))
)
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
	(make-rat (+ (* (numer x)
                  (denom y))
               (* (denom x)
                  (numer y)))
            (* (denom y) (denom x)))  
)

(define (sub-rat x y)
	(make-rat (- (* (numer x)
                  (denom y))
               (* (denom x)
                  (numer y)))
            (* (denom y) (denom x)))  
)

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y)))
)

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x)))
)

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x)))
)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
)

;; 2.2 

(define (last-pair items)
  (let ((cur (car items))
        (next (cdr items)))
    (if (null? next)
        cur
        (last-pair next)))
)

(define (reverse items)
  (let ((cur (car items))
        (next (cdr items)))
    (if (null? next)
        cur
        (cons (reverse next) cur)))
)

(define nil '())

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items))))
)

(define (map-tree proc tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (proc tree))
        (else (cons (map-tree proc (car tree))
                    (map-tree proc (cdr tree)))))
)

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
; 积累
(define (accmulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accmulate op initial (cdr sequence))))
  )

;; 枚举区间
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high)))
  )

; 枚举树的叶子节点
(define (enumerate-trees tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-trees (car tree))
                      (enumerate-trees (cdr tree)))))
)


(define (accmulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accmulate op init (map (lambda (seq) (car seq)) seqs))
	    (accmulate-n op init (map (lambda (seq) (cdr seq)) seqs)))))


(define fold-right accmulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))


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

(define (unique-pair n)
  (flatmap (lambda (x)
	     (map (lambda (y)
		    (list x y))
		  (enumerate-interval 1 (- x 1))))
           (enumerate-interval 2 n)))

(define (prime-sum-pair n)
  (map make-pair-sum
       (filter prime-sum? (unique-pair n))))


;; 2.3

;; 求导函数
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (multiplicand exp)
                         (deriv (multiplier exp) var))))
        (else
          (error "unknown expression type -- DERIV" exp))))

;; 实现构造函数，选择函数，谓词

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x) (and (pair? x) (eq? (car x) '+ )))

;; 被加数
(define (addend x) (cadr x))

;; 加数
(define (augend x) (caddr x))

(define (product? x) (and (pair? x) (eq? (car x) '* )))

(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

;; 进行化简 对构造函数重写

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else
          (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else
          (list '* m1 m2))))

;; 添加幂函数
(define (exponentiation? exp) (and (pair? exp) (eq? (car exp) '** )))

(define (make-exponentiation base exponent)
  (cond ((=number? base 1) 1)
        ((=number? exponent 1) base)
        ((=number? exponent 0) 1)
        (else
          (list '** base exponent))))

(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (multiplicand exp)
                         (deriv (multiplier exp) var))))
        ((exponentiation? exp)
         (make-product
           (make-product
             (exponent exp) (make-exponentiation (base exp) (- (exponent exp) 1)))
           (deriv (base exp) var)))
        (else
          (error "unknown expression type -- DERIV" exp))))

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

;; 集合

;（1）未排序的表
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else
          (element-of-set? x (cdr set)))))

;; 表元素未重复
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else
          (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else
          (cons (car set1)
                (union-set (cdr set1) set2)))))

;; (2) 集合作为排序的表
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else
          (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (next1 (cdr set1))
            (x2 (car set2)) (next2 (cdr set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set next1 next2)))
              ((< x1 x2)
               (intersection-set next1 set2))
              ((< x2 x1)
               (intersection-set set1 next2))))))

(define (adjoin-set x set)
  (if (null? set)
      (list x)
      (let ((fst (car set))
            (next (cdr set)))
        (cond ((= fst x) set)
              ((> fst x)
               (cons fst set))
              ((< fst x)
               (cons fst (adjoin-set x next)))))))


;(3) 集合作为二叉树
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right) (list entry left right))

(define (element-of-tree? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((> x (entry set))
         (element-of-set? x (right-branch set)))
        ((< x (entry set))
         (element-of-set? x (left-branch set)))))

(define (adjoin-tree x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((> x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((< x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

; 将表转化为平衡树
(define (list-tree elements)
  (car (partial-tree elements (length elements))))

;; partial-tree 返回一个序对，第一项为在平衡树中的元素，第二项为不在平衡树中的元素
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;; 将平衡树转化为列表
(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;; Huffman 树

; 创建叶子节点
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf ))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
;; 创建树
(define (make-code-tree left right)
  (list left right (append (symbols left) (symbols right))
                   (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

;; 判断是树还是叶子节点
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; 解码过程 接受0、1表和一棵Huffman树，将0/1表解码为字符
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
                (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                     ; 解码下一个字符
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 1) (right-branch branch))
        ((= bit 0) (left-branch branch))
        (else
          (error "bad bit -- CHOOSE BRANCH" bit))))


;; 带权重元素的集合
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

;; 将符号-权重对偶的表排序
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;; 将字符信息编码为0/1表
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (contain? symbol set)
  (let ((m (memq symbol set)))
    (if (pair? m)
        #t
        #f)))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      '()
      (let ((left (left-branch tree))
            (right (right-branch tree)))
        (let ((left-symbols (symbols left))
              (right-symbols (symbols right)))
          (cond ((contain? symbol left-symbols) (cons 0 (encode-symbol symbol left)))
                ((contain? symbol right-symbols) (cons 1 (encode-symbol symbol right)))
                (else
                  (error "not found symbol in tree -- ENCODE-SYMBOL" symbol)))))))

;; 生成Huffman编码树

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;; 将有序对偶表转化为Huffman树
(define (successive-merge set)
  (if (= (length set) 2)
      (make-code-tree (cadr set) (car set))
      (let ((min1 (car set))
            (min2 (cadr set))
            (next (cddr set)))
        (let ((new (make-code-tree min2 min1)))
          (successive-merge (adjoin-set new next))))))

;; 2.4 抽象数据的多重表示

; 直角坐标系出发
(define (real-part z) (car z))
(define (imag-part z) (cdr z))

;; 利用三角函数公式得到极坐标系
(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(define (angle x) (atan (imag-part z) (real-part z)))

(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a) (cons (* r (cos a))
                                      (* r (sin a))))

;; 从极坐标系出发
(define (magnitude z) (car z))
(define (angle z) (cdr z))

(define (real-part z) (* (magnitude z) (cos (angle z))))
(define (imag-part z) (* (magnitude z) (sin (angle z))))

(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))

(define (make-from-mag-ang r a) (cons r a))

; 创建通用型过程
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))


(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- Type-Tag" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- Contents" datum)))

;; 定义谓词 rectangular? polar?

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular ))

(define (polar? z)
  (eq? (type-tag z) 'polar ))

; 定义带 rectangular 符号的直角坐标系复数形式
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))

;; 半径
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
;; 角度
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular (cons (* r (cos a)) (* r (sin a)))))

; 定义带 polar 符号的极坐标系复数形式
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar (cons (sqrt (+ (square x) (square y)))
                           (atan y x))))

(define (make-from-mag-ang-polar m a) (attach-tag 'polar (cons m a)))


;; 通用型的选择函数 
(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else
         (error "Unknown type -- REAL-PART" z))))

(define (iamg-part z)
  (cond ((rectangular? z) (imag-part-rectangular (contents z)))
        ((polar? z) (imag-part-polar (contents z)))
        (else (error "Unknown type -- IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z) (magnitude-rectangular (contents z)))
        ((polar? z) (magnitude-polar (contents z)))
        (else (error "Unknown type -- MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z) (angle-rectangular (contents z)))
        ((polar? z) (angle-polar (contents z)))
        (else
          (error "Unknown type -- ANGLE" z))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

; ;; 可加性
; (define install-rectangular-package
;   (define (real-part z) (car z))
;   (define (imag-part z) (cdr z))
;   (define (make-from-real-imag x y) (cons x y))
;   (define (magnitude z)
;     (sqrt (+ (square (real-part z))
;              (square (imag-part z)))))
;   (define (angle z)
;     (atan (imag-part z) (real-part z)))
;   (define (make-from-mag-ang r a)
;     (cons (* r (cos a)) (* r (sin a))))
;   (define (tag x) (attach-tag 'rectangular x))
;   (put 'real-part '(rectangular) real-part)
;   (put 'imag-part '(rectangular) imag-part)
;   (put 'magnitude '(rectangular) magnitude)
;   (put 'angle '(rectangular) angle)
;   (put 'make-from-real-imag 'rectangular
;     (lambda (x y) (tag (make-from-real-imag x y))))
;   (put 'make-from-mag-ang 'rectangular
;     (lambda (r a) (tag (make-from-mag-ang r a))))
;   'done
; )

; (define install-polar-package
;   (define (magnitude z) (car z))
;   (define (angle z) (cdr z))
;   (define (make-from-mag-ang r a) (cons r a))
;   (define (real-part z) (* (magnitude z) (cos (angle z))))
;   (define (imag-part z) (* (magnitude z) (sin (angle z))))
;   (define (make-from-real-imag x y)
;     (cons (sqrt (+ square x) (square y))
;           (atan y x)))

;   ;; interface to the rest of the system
;   (define (tag z) (attach 'polar z))
;   (put 'real-part '(polar) real-part)
;   (put 'imag-part '(polar) imag-part)
;   (put 'magnitude '(polar) magnitude)
;   (put 'angle '(polar) angle)
;   (put 'make-from-real-imag 'polar
;        (lambda (x y) (tag (make-from-real-imag x y))))
;   (put 'make-from-mag-ang 'polar 
;        (lambda (r a) (tag (make-from-mag-ang r a))))
;   'done
; )

; (define (apply-generic op . args)
;   (let ((type-tags (map type-tag args)))
;     (let ((proc (get op type-tags)))
;       (if proc
;           (apply proc (map contents args))
;           (error
;             "No method for these types -- APPLY-GENERIC" (list op type-tags))))))

; (define (real-part z) (apply-generic 'real-part z))
; (define (imag-part z) (apply-generic 'imag-part z))
; (define (magnitude z) (apply-generic 'magnitude z))
; (define (angle z) (apply-generic 'angle z))

; (define (make-from-real-imag x y)
;   ((get 'make-from-real-imag 'rectangular ) x y))

; (define (make-from-mag-ang r a)
;   ((get 'make-from-mag-ang 'polar ) r a))

; ;; 消息传递
; (define (make-from-real-imag x y)
;   (define (dispatch op)
;     (cond ((eq? op 'real-part) x)
;           ((eq? op 'imag-part) y)
;           ((eq? op 'magnitude)
;            (sqrt (+ (square x) (square y))))
;           ((eq? op 'angle) (atan y x))
;           (else
;            (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
;   dispatch)

; (define (apple-generic op arg) (arg op))

; ;; 通用型算术运算 用数据导向技术思想实现

; (define (add x y) (apply-generic 'add x y))
; (define (sub x y) (apply-generic 'sub x y))
; (define (mul x y) (apply-generic 'mul x y))
; (define (div x y) (apply-generic 'div x y))


; (define (install-scheme-number-package)
;   (define (tag x)
;     (attach-tag 'scheme-number x))
;   (put 'add '(scheme-number scheme-number)
;        (lambda (x y) (tag (+ x y))))
;   (put 'sub '(scheme-number scheme-number)
;        (lambda (x y) (tag (- x y))))
;   (put 'mul '(scheme-number scheme-number)
;        (lambda (x y) (tag (* x y))))
;   (put 'div '(scheme-number scheme-number)
;        (lambda (x y) (tag (/ x y))))
;   (put 'make 'scheme-number
;        (lambda (x) (tag x)))
;   )

; (define (make-scheme-number n)
;   ((get 'make 'scheme-number) n))

; ;; 有理数包

; (define (install-rational-package)
;   (define (numer x) (car x))
;   (define (denom x) (cdr x))
;   (define (make-rat n d)
;     (let ((g (gcd n d)))
;       (cons (/ n g) (/ d g))))
;   (define (add-rat x y)
;     (make-rat (+ (* (numer x)
; 		    (denom y))
; 		 (* (denom x)
; 		    (numer y)))
; 	      (* (denom y) (denom x)))
; 	)
;   (define (sub-rat x y)
;     (make-rat (- (* (numer x)
; 		    (denom y))
; 		 (* (denom x)
; 		    (numer y)))
; 	      (* (denom y) (denom x)))
;     )
;   (define (mul-rat x y)
;     (make-rat (* (numer x) (numer y))
; 	      (* (denom x) (denom y)))
;     )
;   (define (div-rat x y)
;     (make-rat (* (numer x) (denom y))
; 	      (* (numer y) (denom x)))
;     )
;   (define (equal-rat? x y)
;     (= (* (numer x) (denom y))
;        (* (numer y) (denom x)))
;     )
;   ;; interface to rest of system
;   (define (tag x) (attach-tag 'rational x))
;   (put 'add '(rational rational)
;        (lambda (x y) (add-rat x y)))
;   (put 'sub '(rational rational)
;        (lambda (x y) (sub-rat x y)))
;   (put 'mul '(rational rational)
;        (lambda (x y) (mul-rat x y)))
;   (put 'div '(rational rational)
;        (lambda (x y) (div-rat x y)))
;   (put 'make 'rational
;        (lambda (n d) (tag (make-rat n d))))
;   'done)

; (define (make-rational n d)
;   ((get 'make 'rational) n d))


; ;; 复数运算包

; (define (install-complex-package)
;   (define (make-from-real-imag x y)
;     ((get 'make-from-real-imag 'rectangular) x y))
;   (define (make-from-mag-ang r a)
;     ((get 'make-from-mag-ang 'polar) r a))
;   (define (add-complex z1 z2)
;     (make-from-real-imag (+ (real-part z1) (real-part z2))
;                          (+ (imag-part z1) (imag-part z2))))

;   (define (sub-complex z1 z2)
;     (make-from-real-imag (- (real-part z1) (real-part z2))
;                          (- (imag-part z1) (imag-part z2))))

;   (define (mul-complex z1 z2)
;     (make-from-mag-ang (* (magnitude z1) (magnitude z2))
;                        (+ (angle z1) (angle z2))))

;   (define (div-complex z1 z2)
;     (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
;                        (- (angle z1) (angle z2))))

;   ;; interface to nest of system
;   (define (tag x) (attach-tag 'complex x))
;   (put 'add '(complex complex)
;        (lambda (z1 z2) (tag (add-complex z1 z2))))
;   (put 'sub '(complex complex)
;        (lambda (z1 z2) (tag (sub-complex z1 z2))))
;   (put 'mul '(complex complex)
;        (lambda (z1 z2) (tag (mul-complex z1 z2))))
;   (put 'div '(complex complex)
;        (lambda (z1 z2) (tag (div-complex z1 z2))))
;   (put 'make-from-real-imag 'complex
;        (lambda (x y) (tag (make-from-real-imag x y))))
;   (put 'make-from-mag-ang 'complex
;        (lambda (r a) (tag (make-from-mag-ang r a))))
;   (put 'real-part '(complex) real-part)
;   (put 'imag-part '(complex) imag-part)
;   (put 'magnitude '(complex) magnitude)
;   (put 'angle '(complex) angle)
;   'done)

; (define (make-complex-from-real-imag x y)
;   ((get 'make-from-real-imag 'complex) x y))

; (define (make-complex-from-mag-ang r a)
;   ((get 'make-from-mag-ang 'complex) r a))


; (define type-tower '(scheme-number rational real complex))

(define (monte-carlo trials expriment)
  (define (iterator trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((expriment)
           (iterator (- trials-remaining 1)
                     (+ trials-passed 1)))
          (else
           (iterator (- trials-remaining 1)
                     trials-passed))))
  (iterator trials 0))


