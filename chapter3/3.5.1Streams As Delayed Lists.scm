(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      done'
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))


(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line v)
  (newline)
  (display v))


;;

(define (stream-cons s1 s2) (cons s1 (deley s2)))

(define (stream-car s) (car s))

(define (stream-cdr s) (force (cdr s)))


;; 流实现的行为方式

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval
        (+ low 1)
        high))))


(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else
         (stream-filter pred (stream-cdr stream)))))


;; delay 和 force 的实现

(define (memo-proc proc)
  (let ((already-run? false)
        (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))

(define (force delayed-object)
  (delayed-object))

(define (delay proc)
  (memo-proc (lambda () (proc))))



















;; test

(stream-car
 (stream-cdr
  (stream-filter prime?
                 (stream-enumerate-interval 10000 1000000))))





