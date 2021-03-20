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


;; one-dimensional tables
(define (lookup key table)
  (let ((record (assoc key (cdr tabel))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? (caar records) key) (car records))
        (else
         (assoc key (cdr records)))))


(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value)
                        (cdr table))))
    'ok))

(define (make-table) (list '*table*))

;; two-dimensional tables

(define (lookup2d! key1 key2 table)
  (let ((subtable (assoc key1 (cdr table))))
    (if subtable
        (let ((record (assoc key2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert2d! key1 key2 value table)
  (let ((subtable (assoc key1 (cdr table))))
    (if subtable
        (insert! key2 value subtable)
        (set-cdr! table
                  (cons (list key1 (cons key2 value))
                        (cdr table))))
    'ok))

;; creating local state

(define (make-table-local)
  (let ((local-table (list '*local-table*)))
    (define (lookup key1 key2)
      (let ((subtable (assoc key1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))

    (define (insert key1 key2 value)
      (let ((subtable (assoc key1 (cdr local-table))))
        (if subtable
            (insert! key2 value subtable)
            (set-cdr! local-table
                      (cons (list key1 (cons key2 value))
                            (cdr local-table))))
        'ok))
    (define (dispatch op)
      (cond ((eq? op 'lookup) lookup)
            ((eq? op 'insert) insert)
            (else
             (error "Unknown operation: TABLE" op))))
    dispatch))

(define generation-table (make-table-local))
(define put (generation-table 'insert))
(define get (generation-table 'lookup))

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


(define (front-ptr q) (car q))
(define (rear-ptr q) (cdr q))
(define (set-front-ptr! q item)
  (set-car! q item))
(define (set-rear-ptr! q item)
  (set-cdr! q item))

(define (empty-queue? q)
  (null? (front-ptr q)))

(define (make-queue) (cons '() '()))

(define (front-queue q)
  (if (empty-queue? q)
      (error "FRONT calls with empty queue" q)
      (car (front-ptr q))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (if (empty-queue? queue)
        (begin (set-front-ptr! queue new-pair)
               (set-rear-ptr! queue new-pair)
               queue)
        (begin (set-cdr! (rear-ptr queue) new-pair)
               (set-rear-ptr! queue new-pair)
               queue))))

(define (delete-queue! q)
  (if (empty-queue? q)
      (error "DELETE-QUEUE calls with empty queue" q)
      (begin (set-front-ptr! q (cdr (front-ptr q)))
             q)))

(define (print-queue queue)
  (if (empty-queue? queue)
      '()
      (front-ptr queue)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? (caar records) key) (car records))
        (else
         (assoc key (cdr records)))))

;; one-dimensional tables
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value)
                        (cdr table))))
    'ok))

(define (make-table) (list '*table*))

;; two-dimensional tables

(define (lookup2d! key1 key2 table)
  (let ((subtable (assoc key1 (cdr table))))
    (if subtable
        (let ((record (assoc key2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert2d! key1 key2 value table)
  (let ((subtable (assoc key1 (cdr table))))
    (if subtable
        (insert! key2 value subtable)
        (set-cdr! table
                  (cons (list key1 (cons key2 value))
                        (cdr table))))
    'ok))



(define (make-general-table)
  (let ((table (list '*general-table*)))
    ;
    (define (lookup-general table keys)
      (cond ((null? keys) (error "LOOKUP don't have keys"))
            ((= (length keys) 1)
             (let ((record (assoc (car keys) (cdr table))))
               (if record
                   (cdr record)
                   false)))
            (else
             (let ((subtable (assoc (car keys) (cdr table))))
               (if subtable
                   (lookup-general subtable (cdr keys))
                   #f)))))
    (define (insert-general table value keys)
      (cond ((= (length keys) 0)
             (error "INSER don't calls with keys"))
            ((= (length keys) 1)
             (let ((record (assoc (car keys) (cdr table))))
               (if record
                   (set-cdr! record value)
                   (set-cdr! table
                             (cons (cons (car keys) value)
                                   (cdr table))))
               table))
            (else
             ;; need verify that subtable is exist
             (let ((subtable (assoc (car keys) (cdr table))))
               (if subtable
                   ;; subtable exist -> recursive
                   (insert-general subtable value (cdr keys))
                   ;; subtable don't exist -> create table -> recursive
                   (begin
                     ;; create new-subtable and insert next-table into new-subtable finally insert new-subtable into table
                     (set-cdr! table
                               (cons (insert-general (list (car keys)) value (cdr keys)) (cdr table)))
                     table))))))
    (define (dispatch op)
      (cond ((eq? op 'lookup) (lambda (keys) (lookup-general table keys)))
            ((eq? op 'insert) (lambda (value keys) (insert-general table value keys)))
            (else
             (error "Unknown op -- MAKE_GENERAL_TABLE" op))))
    dispatch))

(define operate-table (make-general-table))
(define (put value . keys) ((operate-table 'insert) value keys))
(define (get . keys) ((operate-table 'lookup) keys))


;; simulator digital circuit


(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e)
    'ok))

(define (full-adder a b in sum out)
  (let ((s (make-wire))
        (c2 (make-wire))
        (c1 (make-wire)))
    (half-adder b in s c1)
    (half-adder a s sum c2)
    (or-gate c2 c1 out)
    'ok))

;; primitive function boxes

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal! input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)


(define (and-gate in1 in2 out)
  (define (and-action-procudure)
    (let ((new-value (logical-and (get-signal! in1) (get-signal! in2))))
      (after-delay
       and-gate-delay
       (lambda () (set-signal! out new-value)))))
  (add-action! in1 and-action-procudure)
  (add-action! in2 and-action-procudure)
  'ok)

(define (or-gate in1 in2 out)
  (define (or-gate-procudure)
    (let ((new-value (logical-or (get-signal! in1) (get-signal! in2))))
      (after-delay
       or-gate-delay
       (lambda () (set-signal! out new-value)))))
  (add-action! in1 or-gate-procudure)
  (add-action! in2 or-gate-procudure)
  'ok)

(define (logical-or in1 in2)
  (cond ((and (= in1 0) (= in2 0)) 0)
        ((or (not (or (= in1 1) (= in1 0)))
             (not (or (= in2 1) (= in2 0)))))
        (else
         1)))

(define (logical-not in)
  (cond ((= in 0) 1)
        ((= in 1) 0)
        (else
         (error "Invalid signal" in))))

(define (logical-and in1 in2)
  (cond ((and (= in1 1) (and in2 1)) 1)
        ((or (not (or (= in1 1) (= in1 0)))
             (not (or (= in2 1) (= in2 0))))
         (error "Invalid sigal" in1 in2))
        (else
         0)))


;; ripper-carry adder
(define (ripper-carry-adder listA listB listS C)
  (define (inner-adder a b c-in s)
    (if (= (length listA) 1)
        (full-adder (car a) (car b) c-in (car s) C)
        (let ((c-out (make-wire)))
          (full-adder (car a) (car b) c-in (car s) c-out)
          (inner-adder (cdr a) (cdr b) c-out (cdr s)))))
  (let ((c-in (make-wire)))
    (set-signal! c-in 0)
    (inner-adder listA listB c-in listS)))


;;Implementing The agenda

(define (make-time-segment time queue) (cons time queue))
(define (segment-time segment) (car segment))
(define (segment-queue segment) (cdr segment))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda) (null? (segments agenda)))


(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))


(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda
                           (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))



;; Representing wire

(define (make-wire)
  (let ((signal-value 0)
        (action-precedure '()))
    (define (set-signal! new-value)
      (if (not (= new-value signal-value))
          (begin (set! signal-value new-value)
                 (call-each action-precedure))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-precedure
            (cons proc action-precedure))
      (proc))
    (define (dispatch op)
      (cond ((eq? op 'get-signal!) signal-value)
            ((eq? op 'set-signal!) set-signal!)
            ((eq? op 'add-action!) accept-action-procedure!)
            (else
             (error "Unknown operation: WIRE" op))))
    dispatch))

(define (set-signal! wire new-value) ((wire 'set-signal!) new-value))
(define (get-signal! wire) (wire 'get-signal!))
(define (add-action! wire action-precedure)
  ((wire 'add-action!) action-precedure))

;; A simple simulation

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name) (display " ")
                 (display (current-time the-agenda))
                 (display " New-value ")
                 (display (get-signal! wire)))))


(define the-agenda (make-agenda))
(define inverter-delay 2)
(define or-gate-delay 3)
(define and-gate-delay 5)

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))


(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

;; propagation constraint
;; implementing the constraint system
;; constraint
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value! a1) (get-value! a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value! sum) (get-value! a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value! sum) (get-value! a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else
           (error "Unknown Request: Adder" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))


(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))


(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value! m1) 0))
               (and (has-value? m2) (= (get-value! m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value! m1) (get-value! m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value! product) (get-value! m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value! product) (get-value! m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else
           (error "Unknown Request: Multiplier" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown Request: CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline) (display "Probo: ") (display name)
    (display " = ") (display value))
  (define (process-new-value)
    (print-probe (get-value! connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else
           (error "Unknown Request: Probe" request))))
  (connect connector me)
  me)


;;

(define (make-connector)
  (let ((value false)
        (informant false)
        (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-expect setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradication" (list value newval)))
            (else
             'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-expect retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'get-value!) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget-value!) forget-my-value)
            ((eq? request 'connect) connect)
            (else
             (error "Unknown operation: CONNECTOR" request))))
    me))



(define (for-each-expect exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else
           (procedure (car items))
           (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value! connector)
  (connector 'get-value!))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget-value!) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) serializer)
            (else (error "Unknown request: MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serializer-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serializer-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serializer-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 ;; 无限排队
                 (the-mutex 'acquire)))
            ((eq? m 'release)
             (clear! cell))))))

(define (clear! cell)
  (set-car! cell false))


(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))
;; test-and-set! 必须是原子性的操作，当一个进程访问互斥元时，发现为 false，那就必须在其他进程访问之前，设置为 true


;; streams


;; delay 和 force 的实现

(define (memo-proc proc)
  (let ((already-run? false)
        (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))

(define (force delayed-object)
  (delayed-object))

;(define (delay exp)
;  (memo-proc (lambda () exp)))

;(define (cons-stream s1 s2) (cons s1 (delay s2)))

(define-syntax delay
  (syntax-rules ()
    ((_ exp) (memo-proc (lambda () exp)))))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (stream-car s) (car s))

(define (stream-cdr s) (force (cdr s)))

(define (stpream-null? s) (null? s))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      done'
      (begin
        (proc (stream-car s))
        (stream-for-each proc (stream-cdr s)))))

(define (display-line v)
  (newline)
  (display v))

(define (display-stream s)
  (stream-for-each display-line s))

;; 流实现的行为方式
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval
        (+ low 1)
        high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else
         (stream-filter pred (stream-cdr stream)))))

;; 无穷流
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))


(define integers (integers-starting-from 1))

(define (divisible? x y)
  (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

;;厄拉多塞筛法
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))

;; 隐式的定义流

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))

(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))


(define (take n stream)
  (define (iter i)
    (if (= i (- n 1))
        (stream-ref stream (- n 1))
        (cons (stream-ref stream i) (iter (+ i 1)))))
  (iter 0))

(define (takeWhile condition stream)
  (let ((v (stream-car stream)))
    (if (condition v)
        (stream-car stream)
        (cons v (takeWhile condition (stream-cdr stream))))))

(define (partial-sum s)
  (cons-stream (stream-car s) (add-streams (stream-cdr s) (partial-sum s))))

;; 系统地将迭代操作转化为流操作

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sum (pi-summands 1)) 4))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

;; 递归的加速
;; 得到以流为单位的流
(define (make-tableua transform s)
  (cons-stream s
               (make-tableua transform
                             (transform s))))

;; 取流中单位的第一项
(define (accelerated-squence transform s)
  (stream-map stream-car (make-tableua transform s)))

(define (reverse-list l)
  (define (iter listItem res)
    (if (null? listItem)
        res
        (let ((cur (car listItem))
              (next (cdr listItem)))
          (iter next (append (list cur) res)))))
  (iter l '()))

(define (takeWhile condition stream)
  (define (iter stream res)
    (let ((cur (stream-car stream)))
      (if (condition cur)
          (iter (stream-cdr stream) (append (list cur) res))
          (reverse-list res))))
  (iter stream '()))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave ;;以某种方式组合
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define int-pairs (pairs integers integers))


(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((car-s1 (stream-car s1))
               (car-s2 (stream-car s2)))
           (if (weight car-s1 car-s2)
               (cons-stream car-s1 (merge-weighted (stream-cdr s1) s2 weight))
               (cons-stream car-s2 (merge-weighted s1 (stream-cdr s2) weight)))))))

(define (pairs-weight s1 s2 weight)
  (cons-stream
   (list (stream-car s1) (stream-car s2))
   (merge-weighted
    (merge-weighted
     (stream-map (lambda (x) (list x (stream-car s2)))
                 (stream-cdr s1))
     (stream-map (lambda (x) (list (stream-car s1) x))
                 (stream-cdr s2))
     weight)
    (pairs-weight (stream-cdr s1) (stream-cdr s2) weight)
    weight)))

(define (filter-single stream compare)
  (define (iter s prevList)
    (let ((cur (stream-car s))
          (next (stream-cdr s)))
      (if (compare cur (car prevList))
          (iter next (cons cur prevList))
          (if (= (length prevList) 1)
              (iter next (list cur))
              (cons-stream
               prevList
               (iter next (list cur)))))))
  (iter (stream-cdr stream) (list (stream-car stream))))


(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define random-init 42)

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (rand-update x)
  (define a 13)
  (define b 17)
  (define m 19)
  (remainder (+ (* a x) b) m))

(define random-numbers
  (cons-stream random-init
               (stream-map rand-update
                           random-numbers)))

;; 非确定性计算
(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else
         (distinct? (cdr items)))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
