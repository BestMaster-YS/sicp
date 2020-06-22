(define nil '())

(define (same-parity first . next)
  (define (filter f list predicate)
    (cond ((null? list) nil)
          ((= (remainder (car list) 2) f)
           (cons (car list) (filter f (cdr list) predicate)))
          (else
            (filter f (cdr list) predicate)))
  )
  (cons first (filter first next (remainder first 2)))
)

(define (same-parity first . rest)
  (define (same-parity-iter source dist val)
    (if (null? source)
        dist
        (same-parity-iter (cdr source)
                          (if (= (remainder (car source) 2) val)
                              (append dist (list (car source)))
                              dist)
                          val))
  )
  (same-parity-iter rest (list first) (remainder first 2))
)

(same-parity 1 2 3 4 5 6)