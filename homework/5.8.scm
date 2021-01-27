(define insts
  (start
   (goto (label here))
   here
   (assign a (const 3))
   (goto (label there))
   here
   (assign a (const 4))
   (goto (label there))
   there))


(define (check-has-same-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        true
        false)))

(define (extract-labels test receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ((next-inst (car text)))
                          (if (symbol? next-inst)
                              (if (check-has-same-label next-inst labels)
                                  (error "INSTS HAS SAME LABELS --- EXTRACT-LABELS")
                                  (receive insts
                                      (cons (make-label-entry next-inst
                                                              insts)
                                            labels)))
                              (receive (cons (make-instruction next-inst)
                                             insts)
                                  labels)))))))



