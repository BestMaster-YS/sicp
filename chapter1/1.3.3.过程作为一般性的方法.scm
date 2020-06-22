(load "util.scm")

(define (search f neg-point pos-point)
  (let ((mid-point (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        mid-point
        (let ((test-value (f mid-point)))
          (cond ((positive? test-value)
                 (newline)
                 (display mid-point)
                 (search f neg-point mid-point))
                ((negative? test-value)
                 (newline)
                 (display mid-point)
                 (search f mid-point pos-point))
                (else
                  mid-point)))))
)

(define (close-enough? a b)
  (< (abs (- a b)) 0.001)
)

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (positive? a-value) (negative? b-value))
           (search f b a))
          ((and (positive? b-value) (negative? a-value))
           (search f a b))
          (else
           (error "Values are not of opposite sign" a b))))
)


(half-interval-method sin 2.0 4.0)

; 寻找函数的不动点

(define tolerance 0.00001)

(define (fix-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance)
  )

  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next)))
  )
  (try first-guess)
)

(fix-point cos 1.0)

(fix-point (lambda (x) (+ (sin x) (cos x))) 1.0)

(define (sqrt x)
	(fix-point (lambda (y) (average y (/ x y))) 1.0)
)

(sqrt 9)
