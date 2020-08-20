(define (make-frame variables values)
  (if (= (length variables) (length values))
      (map cons variables values)
      (error "length mismatch -- MAKE-FRAME" variables values)))

(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))

(define (add-binding-to-frame! var val frame)
  (cons (cons var val) frame))


