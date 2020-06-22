(define (add-interval a b)
  (make-interval (+ (lower-bound a) (lower-bound b))
                 (+ (upper-bound a) (upper-bound b)))
)

(define (make-interval a b) (cons a b))

;; 需要规定 lower-bound 为第一个，upper-bound为第二个
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

;; 则无需限制
(define (lower-bound interval) (min (car interval) (cdr interval)))
(define (upper-bound interval) (max (car interval) (cdr interval)))

(define (mul-interval a b)
  (let ((p1 (* (lower-bound a) (lower-bound b)))
        (p2 (* (lower-bound a) (upper-bound b)))
        (p3 (* (upper-bound a) (lower-bound b)))
        (p4 (* (upper-bound a) (upper-bound b))))
    (make-interval (max p1 p2 p3 p4)
                   (min p1 p2 p3 p4)))
)

(define (div-interval a b)
  (mul-interval a
                (make-interval (/ 1.0 (upper-bound b))
                               (/ 1.0 (lower-bound b))))
)