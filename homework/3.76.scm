(load "3.74.scm")

(define (smooth-stream input-stream)
  (define (iter cur next next-stream)
    (cons-stream
     (average cur next)
     (iter next (stream-car next-stream) (stream-cdr next-stream))))
  (let ((f (stream-car input-stream))
        (ns (stream-cdr input-stream)))
    (iter f (stream-car ns) (stream-cdr ns))))

(define zero-crossings
  (let ((smoothed (smooth-stream sense-data)))
    (stream-map sign-change-detector smoothed
                (cons-stream 0 smoothed))))


