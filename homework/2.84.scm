(load "util.scm")

;; 没有讲到实数
(define tower '(scheme-number rational real complex))

;; 遍历类型塔，并返回相同类型的值列表
(define (compare-raise-type v1 v2)
  (define (iterate v1 v2 t)
    (let ((type1 (type-tag v1))
          (type2 (type-tag v2))
          (bottom (car t)))
      (cond ((eq? type1 type2)
             (list v1 v2))
            ((eq? type1 bottom)
             (iterate (raise v1) v2 (cdr t)))
            ((eq? type2 bottom)
             (iterate v1 (raise v2) (cdr t)))
            (else
             (iterate v1 v2 (cdr t))))))
  (iterate v1 v2 tower))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (apply (lambda (v1 v2)
                       (apply-generic op v1 v2))
                     (apply compare-raise-type args))
              (error "No method for these types"
                     (list op type-tags)))))))
