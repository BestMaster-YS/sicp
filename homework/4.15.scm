(define (run-forever) (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

;; 图灵的停机定理

(halts? try try)

;; if (try try) is halted (run-forever) not halted
;; if (try try) is not halted, is halted

