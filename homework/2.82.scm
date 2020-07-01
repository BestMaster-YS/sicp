;;


(define (apply-generic op . args)
  (define types (map type-tag args))

  (define (try-coerce-to target)
    (map (lambda (v)
           (let ((coercoin-proc (get-coercoin (type-tag v) (type-tag target))))
             (if coercoin-proc
                 (coercoin-proc v target)
                 v)))
         args))

  (define (iterate next)
    (if (null? type-list)
        (error "No coercion strategy for these types" (list op types))
        (let ((coreced (try-coerce-to (car next)))
              (rest (cadr next)))
          (let ((proc (get op (try-coerce-to target))))
            (if proc
                (apply proc (map contents args))
                (iterate rest))))))


  (let ((proc (get op types)))
    (if proc
        (apply op (map contents args))
        ;; 进行类型强制转换
        (iterate args)))

  )


;; 设有类型 A B C
;; 函数 c(A, A, A), 只有 A -> B, C -> B 的强制类型转换




