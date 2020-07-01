(define (add-complex-to-schemenum z x)
  (make-from-real-imag (+ (real-part z) x)
                       (imag-part z)))

(put 'add '(complex scheme-number)
     (lambda (z x) (tag (add-complex-to-schemenum z x))))

;; 虽然可用但是非常麻烦

;; 强制方法，例如将正常数看成虚部为0的复数

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)


;; 重新实现 apply-generic，使其能使用强制类型转换

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          ;; 不同类型组合
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond ((t1->t2
                          (apply-generic op (t1->t2 a1) a2))
                         (t2->t1
                          (apply-generic op (t2->t1 a2) a1))
                         (else
                          (error "No method for these types"
                                 (list op type-tags)))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;; 类型的层次结构


;; 层次结构的不足



