(load "2.12.scm")

;; p3 = (p1 + p2) / (1 + p1p2)

;; 并联电阻 R1 R2 两种等价方式

(define (par1 R1 R2)
  (div-interval (mul-interval R1 R2)
                (add-interval R1 R2))
)

(define (par2 R1 R2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one R1)
                                (div-interval one R2))))
)

(define a (make-center-percent 10 0.1))
(define b (make-center-percent 15 0.1))

(center (par1 a b))
(percent (par1 a b))

(center (par2 a b))
(percent (par2 a b))
