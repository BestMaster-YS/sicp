(load "util.scm")

(define symbol-sets '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))

(define message
  '(Get a job 
    Sha na na na na na na na na 
    Get a job 
    Sha na na na na na na na na 
    Wah yip yip yip yip yip yip yip yip yip 
    Sha boom))


(length (encode message (generate-huffman-tree symbol-sets)))
