;; church 计数

(define zero (lambda (f) (lambda (x) x)))

(define (add-one n)
  (lambda (f)
          (lambda (x)
                  (f ((n f) x))
          )
  )
)

(define one (lambda (f) (lambda (x) (f x))))

;; (f ((one f) x)) = (f (f x))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define three (lambda (f) (lambda (x) (f (f (f x))))))


(define plus (n1 n2)
  (lambda (f)
          (lambda (x)
                  ((n2 f) ((n1 f) x))
                  ))
)

;; n1 = n n2 = one
(define (add-one n) (plus one n))
(define (add-one n)
  (lambda (f) (lambda (x)
                      ((n2 f) ((n f) x))
                      ))
)

