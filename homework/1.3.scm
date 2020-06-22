(define (max a b)
  (if (> a b)
      a
      b))

(define (the-two-max-sum-in-three a b c)
  (if (> a b)
      (+ a (max b c))
      (+ b (max a c))))

(the-two-max-sum-in-three 1 2 3)