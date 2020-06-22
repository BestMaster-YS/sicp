(define (for-each f items)
  (if (null? items)
      #t
      ((lambda (x)
               (f x)
               (for-each f (cdr items))) (car items)))
)

(for-each (lambda (x) (newline) (display x)) (list 1 2 3))