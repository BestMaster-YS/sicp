(load "util.scm")

(define (>= x y)
  (or (> x y) (= x y)))

(define (sign-change-detector x y)
  (cond ((or (and (< x 0) (< y 0))
             (and (> x 0) (> y 0))
             (and (= x 0) (= y 0)))
         0)
        ((and (< x 0) (>= y 0)) -1)
        ((and (>= x 0) (< y 0)) 1)))

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

(define zero-crossings (make-zero-crossings sense-data 0))

(define zero-crossings
  (stream-map sign-change-detector sense-data
              (cons-stream 0 sense-data)))

