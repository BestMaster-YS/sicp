(define (make-mobile left right)
  (list left right)
)

(define (left-branch m) (car m))
(define (right-branch m) (cadr m))

(define (make-branch length structure)
  (list length structure)
)

(define (branch-length m) (car m))
(define (branch-structure m) (cadr m))

(define (total m)
  (cond ((null? m) 0)
        ((not (pair? m)) m)
        (else
          (+ (total (branch-structure (left-branch m)))
             (total (branch-structure (right-branch m))))))
)

(define a (make-mobile (make-branch 2 3) (make-branch 2 3)))

(total a)

(define (blance? mobile)
  (define (blance-helper mobile)
    (let ((l-v (branch-structure (left-branch mobile)))
          (r-v (branch-structure (right-branch mobile)))
          (l-length (branch-length (left-branch mobile)))
          (r-length (branch-length (right-branch mobile))))
      (cond ((and (pair? l-v) (pair? r-v))
             (cons (and (car (blance-helper l-v)) (car (blance-helper r-v))
                        (= (* r-length (cdr (blance-helper r-v))) (* l-length (cdr (blance-helper l-v)))))
                    (+ (cdr (blance-helper r-v)) (cdr (blance-helper l-v)))))
            ((pair? l-v)
             (cons (and (= (* r-v r-length) (* l-length (cdr (blance-helper l-v))))
                        (car (blance-helper l-v))))
                   (+ r-v (cdr (blance-helper l-v))))
            ((pair? r-v)
             (cons (and (= (* l-v l-length) (* r-length (cdr (blance-helper r-v))))
                        (car (blance-helper r-v))))
                   (+ l-v (cdr (blance-helper r-v))))
            (else
              (cons (= (* r-v r-length) (* l-v l-length))
                    (+ l-v r-v)))))
  )
  (car (blance-helper mobile))
)

(define d (make-mobile (make-branch 10 a) (make-branch 12 5)))

(blance? a)
(blance? d)
