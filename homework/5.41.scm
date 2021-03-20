(define (indexof a listA)
  (define (iter curL i)
    (if (null? curL)
        -1
        (let ((cur (car curL)))
          (if (eq? a cur)
              i
              (iter (cdr curL) (+ i 1))))))
  (iter listA 0))

(define (find-variable variable compile-time-env)
  (define (iter rest-frame idx)
    (if (null? rest-frame)
        'not-found
        (let ((inner-idx (indexof variable (car rest-frame))))
          (if (eq? inner-idx -1)
              (iter (cdr rest-frame) (+ idx 1))
              (cons idx inner-idx)))))



