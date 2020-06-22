(load "util.scm")

;;

;; 求幂 fast-expt 需处理 n = 0 的情形
(define (expt base n)
  (if (= n 0)
      1
      (fast-expt base n))
)

(expt 2 5)

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

(define (test nth times)
  ((damped-nth-root nth times) (expt 2 nth))
)
;; nth  damp-times
;; 2    1
(test 2 1)

;; 3    1
(test 3 1)

;; 4    2
(test 4 2)

;; 5    2
(test 5 2)

;; 6    2
(test 6 2)

;; 7    2
(test 6 2)

;; 8    3
(test 8 3)

;; 9    3
(test 9 3)

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

((nth-root 10) 1024)