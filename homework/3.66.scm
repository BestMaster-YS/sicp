(load "util.scm")

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))


(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave ;;以某种方式组合
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))


(define int-pairs (pairs integers integers))


(define prime-sum-stream (stream-filter (lambda (pair)
                                           (prime? (+ (car pair) (cadr pair))))
                                        int-pairs))

(define (reverse-list l)
  (define (iter listItem res)
    (if (null? listItem)
        res
        (let ((cur (car listItem))
              (next (cdr listItem)))
          (iter next (append (list cur) res)))))
  (iter l '()))

(define (takeWhile condition stream)
  (define (iter stream res)
    (let ((cur (stream-car stream)))
      (if (condition cur)
          (iter (stream-cdr stream) (append (list cur) res))
          (reverse-list res))))
  (iter stream '()))

(takeWhile (lambda (pair)
             (not (equal? pair '(1 100))))
           int-pairs)

(length (takeWhile (lambda (pair)
                     (not (equal? pair '(1 100))))
                   int-pairs))

