(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))

;; 1 : 1000000
;; 2 : 100
;; 3 : 1000
;; 4 : 10000
;; 5 : 100000

(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))

; 1000000
