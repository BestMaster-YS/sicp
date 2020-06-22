(load "util.scm")

(define (make-rat n d)
  (cond ((and (positive? n) (positive? d))
         (let ((g (gcd n d)))
           (cons (/ n g) (/ d g))))
        ((positive? n)
         (let ((g (gcd n (- d))))
           (cons (- (/ n g))
                 (/ (- d) g))))
        (else
          (let ((g (gcd (- n) d)))
            (cons (- (/ (- n) g))
                  (/ d g)))))
)


(print-rat (make-rat (- 10) 12))
(print-rat (make-rat 10 (- 12)))
