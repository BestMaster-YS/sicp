(define (project x) (apply-generic 'project x))

(define (type-tower) '(scheme-number rational real complex))

;; porject :: real -> rational
(put 'project 'complex
     (lambda (c)
       (make-real (real-part c))))

(put 'project 'real
     (lambda (r)
       (make-rational (floor r) 1)))

(put 'project 'rational
     (lambda (n)
       (make-scheme-number (numer n))))

(define (drop n)
  (define (try-lower n)
    (let ((project-next (get 'porject (type-tag n))))
      (if project-next
          (let ((drop-n (project-next n))
                (raise-cur (raise n)))
            (if (equ? drop-n raise-cur)
                (try-lower drop-n)
                n))
          ;; 不能再下降类型
          n))))

;; 遍历类型塔，并返回相同类型的值列表
;; 首先要达到相同级别的类型，drop 会将类型下降到最低
;; step0: 与类型塔 bottom 比较，同时相等 --> 退出
;; step1: 两原类型不想等于 bottom  --> 同时 drop --> 比较 -> 相等 --> 返回 drop 的类型
;; step2: 不相等 -> raise 较低级别的类型，若是 raise后的类型相等 --> 则成功退出
;; step3: 不相等 -> step2

(define (compare-type type1 type2)
  (define (iterate t1 t2 t)
    (if (null? t)
        (error
         "type tower don't have these type" (list t1 t2))
        (let ((bottom (car t)))
          (cond ((eq? t1 t2) #f)
                ((eq? t1 bottom) #f)
                ((eq? t2 bottom) #t)
                (else
                 (iterate t1 t2 (cdr t)))))))
  (iterate type1 type2 type-tower))


(define (raise-to-eq-type v1 v2)
  (let ((t1 (type-tag v1))
        (t2 (type-tag v2)))
    (cond ((eq? t1 t2) (list t1 t2))
          ((compare-type t1 t2) (raise-to-eq-type v1 (raise v2)))
          (else
           (raise-to-eq-type (raise v1) v2)))))

(define (compare-drop-type v1 v2)
  (let ((drop-v1 (drop v1))
        (drop-v2 (drop v2))
        (drop-type1 (type-tag drop-v1))
        (drop-type2 (type-tag drop-v2)))
    (if (eq? drop-v1 drop-v2)
        (list drop-v1 drop-v2)
        (raise-to-eq-type drop-v1 drop-v2))))

(define (apply-generic op . args)
  (if (= (length args) 2)
      (let ((eq-args (apply compare-drop-type args)))
        (let ((type-tags (map type-tag eq-args)))
          (let ((proc (get op type-tags)))
            (apply proc (map contents eq-args)))))
      (error
       "args is beyound 2" args)))

