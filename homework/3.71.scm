(load "util.scm")

(define (cube-sum seq) (+ (cube (car seq)) (cube (cadr seq))))

(define (cube-weight seq1 seq2)
  (< (cube-sum seq1) (cube-sum seq2)))

(define (filter-single stream compare)
  (define (iter s prevList)
    (let ((cur (stream-car s))
          (next (stream-cdr s)))
      (if (compare cur (car prevList))
          (iter next (cons cur prevList))
          (if (= (length prevList) 1)
              (iter next (list cur))
              (cons-stream
               prevList
               (iter next (list cur)))))))
  (iter (stream-cdr stream) (list (stream-car stream))))

(define ramanujan-stream
  (filter-single
   (stream-map (lambda (seq) (cons (cube-sum seq) seq))
               (pairs-weight integers integers cube-weight))
   (lambda (s1 s2)
     (and (= (car s1) (car s2))
          (not (= (cadr s1) (cadr s2)))
          (not (= (cadr s1) (caddr s2)))))))

(take 5 ramanujan-stream)
