;; ripper-carry adder

(define (ripper-carry-adder listA listB listS C)
  (define (inner-adder a b c-in s)
    (if (= (length listA) 1)
        (full-adder (car a) (car b) c-in (car s) C)
        (let ((c-out (make-wire)))
          (full-adder (car a) (car b) c-in (car s) c-out)
          (inner-adder (cdr a) (cdr b) c-out (cdr s)))))
  (let (c-in (make-wire))
    (set-signal! c-in 0)
    (inner-adder listA listB c-in listS)))



