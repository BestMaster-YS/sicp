(load "../homework/util.scm")

(define random-init 42)

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (rand-update x)
  (define a 13)
  (define b 17)
  (define m 19)
  (remainder (+ (* a x) b) m))

;; use monte-carlo simulation to approximate π
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

;(define (rand) (random 1000))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials expriment)
  (define (iterator trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((expriment)
           (iterator (- trials-remaining 1)
                     (+ trials-passed 1)))
          (else
            (iterator (- trials-remaining 1)
                      trials-passed))))
  (iterator trials 0))

(estimate-pi 10000)

;; using random-update
(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))

(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x))
          (x2 (rand-update x)))
      (cond ((= trials-remaining 0)
             (/ trials-passed trials))
            ((= (gcd x1 x2) 1)
             (iter (- trials-remaining 1)
                   (+ trials-passed 1)
                   x2))
            (else
             (iter (- trials-remaining 1)
                   trials-passed
                   x2)))))
  (iter trials 0 initial-x))

(estimate-pi 10000)

