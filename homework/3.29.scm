;; construct an or-gate as a compound digital logical device, built from and-gate and inverters.


(define (or-gate in1 in2 out)
  (define (inner in out)
    (let ((a1 (make-wire))
          (a2 (make-wire)))
      (inverter in a1)
      (inverter in a2)
      (and-gate a1 a2 out)))
  (let ((inner-out1 (make-wire))
        (inner-out2 (make-wire))
        (a3 (make-wire))
        (a4 (make-wire)))
    (inner in1 inner-out1)
    (inner in2 inner-out2)
    (and-gate inner-out1 inner-out2 a3)
    (inverter a3 out)
    'ok))

