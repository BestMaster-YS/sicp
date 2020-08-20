(define (or-gate in1 in2 out)
  (define (or-gate-procudure)
    (let ((new-value (logical-or (get-signal in1) (get-signal in2))))
      (after-delay
       or-gate-delay
       (lambda () (set-signal! out new-value)))))
  (add-action! in1 or-gate-procudure)
  (add-action! in2 or-gate-procudure)
  'ok)

(define (logical-or in1 in2)
  (cond ((and (= in1 0) (= in2 0)) 0)
        ((or (not (or (= in1 1) (= in1 0)))
             (not (or (= in2 1) (= in2 0)))))
        (else
         1)))

