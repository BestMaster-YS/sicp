(load "util.scm")


(define (message max)

  (let ((mutex (make-mutex)))

    (define (acquire)
      (mutex 'acquire)
      (if (> max 0)
          (begin (set! max (- max 1))
                 (mutex 'release)
                 'ok)
          (acquire)))

    (define (release)
      (mutex 'acquire)
      (set! max (+ max 1))
      (mutex 'release)
      'ok)

    (define (dispatch op)
      (cond ((eq? op 'acquire) (acquire))
            ((eq? op 'release) (release))
            (else
             (error "Unknown Operation: Message" op))))

    )
  )



(define (message max)

  (let ((mutex (make-mutex)))

    (define (acquire)
      (if (atom max)
          (acquire)
          'ok))

    (define (release)
      (set! max (+ max 1))
      'ok)

    (define (dispatch op)
      (cond ((eq? op 'acquire) (acquire))
            ((eq? op 'release) (release))
            (else
             (error "Unknown Operation: Message" op))))

    )
  )

;; 原子性操作，atom
(define (atom n)
  (if (= n 0)
      #t
      (begin (set! n (- n 1))
             #f)))



;; mit-scheme 实现原子性操作
(define (atom n)
  (without-interrupts
   (lambda ()
     (if (= n 0)
         #t
         (begin (set! n (- n 1))
                #f)))))



