(load "util.scm")

(define (square x) (* x x))

(define (square-sum seq) (+ (square (car seq)) (square (cadr seq))))

(define (square-weight seq1 seq2)
  (< (square-sum seq1) (square-sum seq2)))

;; 数据量太大，会导致 stream-filter 爆栈
(define (pairs-weight s1 s2 weight)
  (cons-stream
   (list (stream-car s1) (stream-car s2))
   (merge-weighted
     (stream-map (lambda (x) (list x (stream-car s2)))
                 (stream-cdr s1))
     (pairs-weight (stream-cdr s1) (stream-cdr s2) weight)
     weight)))

(define square-difference-three-time
  (stream-filter
   (lambda (seq) (= (length seq) 3))
   (filter-single
    (stream-map (lambda (seq) (cons (square-sum seq) seq))
                (pairs-weight integers integers square-weight))
    (lambda (s1 s2)
      (and (= (car s1) (car s2))
           (not (= (cadr s1) (cadr s2)))
           (not (= (cadr s1) (caddr s2))))))))

(take 5 square-difference-three-time)

