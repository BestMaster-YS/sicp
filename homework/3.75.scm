(load "3.74.scm")

(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((cur-avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector cur-avpt last-avpt)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt))))



