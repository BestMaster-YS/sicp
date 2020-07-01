(define (scheme-number->scheme->number n) n)
(define (complex->complex z) z)

(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme->number)
(put-coercion 'complex 'complex
              complex->complex)

;; (a)
;; 定义通用型 exp 操作
(define (esp x y) (apply-generic 'exp x y))
;; 只放入 scheme-number 包中
(put 'exp '(scheme-number scheme->number)
     (lambda (x y) (tag (expt x y))))

;; 对两个复数调用 exp 则会无限调用 complex->complex 过程，因为在复数包中没有对应的操作，需要进行类型转换，本来是没有遇到两个complex时

;; (b)
;; 没有，不能


;; (c)

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
                (if (eq? type1 type2)
                    (error "Don't coercion two same types" (list type1 type2))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond ((t1->t2
                              (apply-generic op (t1->t2 a1) a2))
                             (t2->t1
                              (apply-generic op (t2->t1 a2) a1))
                             (else
                              (error "No method for these types"
                                     (list op type-tags))))))))
              (error "No method for these types"
                     (list op type-tags)))))))


