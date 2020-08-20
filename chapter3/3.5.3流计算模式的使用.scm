(load "../homework/util.scm")
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

(take 10 (sqrt-stream 2))


(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sum (pi-summands 1)) 4))

(take 10 pi-stream)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(take 10 (euler-transform
          pi-stream))

;; 递归的加速
;; 得到以流为单位的流
(define (make-tableua transform s)
  (cons-stream s
               (make-tableua transform
                             (transform s))))

;; 取流中单位的第一项
(define (accelerated-squence transform s)
  (stream-map stream-car (make-tableua transform s)))

(take 20 (accelerated-squence euler-transform pi-stream))

;; 序对的无穷流

;; (s0, t0) | (s0, t1) (s0, t2)...
;; --------------------------------
;;          | (s1, t1) (s1, t2)
;;          |          (s2, t2)

;; 该方法组合不能遍历两个流所有的元素
(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

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


(stream-filter (lambda (pair)
                 (prime? (+ (car pair) (cadr pair))))
               int-pairs)

;; 流作为信号使用

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)



