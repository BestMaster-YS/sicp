(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      #f
      (let ((cur (entry set-of-records))
            (left (left-branch set-of-records))
            (right (right-branch set-of-records)))
        (cond ((= (key cur) given-key) cur)
              ((> (key cur) given-key) (lookup given-key right))
              ((< (key cur) given-key) (lookup given-key left))))))