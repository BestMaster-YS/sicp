;;
(define (and-gate in1 in2 out)
  (define (and-action-procudure)
    (let ((new-value (logical-and (get-signal in1) (get-signal in2))))
      (after-delay
       and-gate-delay
       (lambda () (set-signal! out new-value)))))
  (add-action! in1 and-action-procudure)
  (add-action! in2 and-action-procudure)
  'ok)

(define (accept-action-procedure! proc)
  (set! action-precedure
        (cons proc action-precedure))
  (proc))

;;The add-action-procedure is a function that returns a function to set wire a new signal. your truly a goal that is set wire signal, so we should execute the procedure initially that gets from accept-action-procedure arguments.
