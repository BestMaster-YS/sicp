(load "util.scm")

;; 首先从 1 1 1 开始
;; 如果 i^2 + j^2 > k^2 不用继续往后找了
;; 先得到 pairs

(define (triples s t u)
  (cons-stream (list
                (stream-car s)
                (stream-car t)
                (stream-car u))
               (interleave
                (stream-map (lambda (x) (cons (stream-car s) x))
                            (stream-cdr (pairs t u)))
                (triples (stream-cdr s)
                         (stream-cdr t)
                         (stream-cdr u)))))

(define pythagorean-tripes
  (stream-filter
   (lambda (triple)
     (let ((t (map square triple)))
       (= (+ (car t) (cadr t)) (caddr t))))
   (triples integers integers integers)))

(stream-ref pythagorean-tripes 0)
(stream-ref pythagorean-tripes 1)

;(take 5 pythagorean-tripes)
