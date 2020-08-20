(load "util.scm")

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((car-s1 (stream-car s1))
               (car-s2 (stream-car s2)))
           (if (weight car-s1 car-s2)
               (cons-stream car-s1 (merge-weighted (stream-cdr s1) s2 weight))
               (cons-stream car-s2 (merge-weighted s1 (stream-cdr s2) weight)))))))

(define (pairs-weight s1 s2 weight)
  (cons-stream
   (list (stream-car s1) (stream-car s2))
   (merge-weighted
    (merge-weighted
     (stream-map (lambda (x) (list x (stream-car s2)))
                 (stream-cdr s1))
     (stream-map (lambda (x) (list (stream-car s1) x))
                 (stream-cdr s2))
     weight)
    (pairs-weight (stream-cdr s1) (stream-cdr s2) weight)
    weight)))

;; (a)

(define (weight-a seq1 seq2)
  (if (> (accmulate + 0 seq1) (accmulate + 0 seq2))
      #f
      #t))

(define a-stream
  (stream-filter
   (lambda (x) (< (car x) (cadr x)))
   (pairs-weight integers integers weight-a)))

(take 10 a-stream)


;; (b)
(define (sum-b seq)
  (+ (* 2 (car seq)) (* 3 (cadr seq)) (* 5 (car seq) (cadr seq))))

(define (weight-b seq1 seq2)
  (if (> (sum-b seq1) (sum-b seq2))
      #f
      #t))

(define (divisible-2-3-5 a)
  (or (= (remainder a 2) 0)
      (= (remainder a 3) 0)
      (= (remainder a 5) 0)))

(define b-stream
  (stream-filter
   (lambda (seq) (or (divisible-2-3-5 (car seq)) (divisible-2-3-5 (cadr seq))))
   (pairs-weight integers integers weight-b)))

(take 10 b-stream)
