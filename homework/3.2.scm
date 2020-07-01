;(load "util.scm")

(define (make-monitor f)
  (let ((count 0))
    (lambda (arg)
      (cond ((eq? arg 'how-many-calls? )
             count)
            ((eq? arg 'reset-count )
             (set! count 0))
            (else
              (begin (set! count (+ count 1))
                     (f arg)))))))

(define a (make-monitor sqrt))

(a 100)

(a 'how-many-calls? )
