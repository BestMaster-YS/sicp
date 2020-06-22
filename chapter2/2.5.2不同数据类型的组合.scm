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
    (let ((pro (get op type-tags))))))


