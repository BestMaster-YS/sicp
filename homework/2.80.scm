(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))


(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  )

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;; 有理数包

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x)
		    (denom y))
		 (* (denom x)
		    (numer y)))
	      (* (denom y) (denom x)))
	)
  (define (sub-rat x y)
    (make-rat (- (* (numer x)
		    (denom y))
		 (* (denom x)
		    (numer y)))
	      (* (denom y) (denom x)))
    )
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
	      (* (denom x) (denom y)))
    )
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
	      (* (numer y) (denom x)))
    )
  (define (equal-rat? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x)))
    )
  ;; interface to rest of system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (add-rat x y)))
  (put 'sub '(rational rational)
       (lambda (x y) (sub-rat x y)))
  (put 'mul '(rational rational)
       (lambda (x y) (mul-rat x y)))
  (put 'div '(rational rational)
       (lambda (x y) (div-rat x y)))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put '=zero? 'rational
       (lambda (n) (and (= (numer n) 0)
                        (not (= (denom n) 0)))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))


;; 复数运算包

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))

  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))

  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))

  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ;; interface to nest of system
  (define (tag x) (attach-tag 'complex x))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'rectangular '(complex)
       (lambda (z) (and (= (real-part z) 0)
                        (= (imag-part z) 0))))
  (put 'polar '(complex)
       (lambda (z) (= (magnitude z) 0)))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


(define (=zero? n)
  (apply-generic '=zero? n)

