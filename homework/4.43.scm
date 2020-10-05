(load "4.41.scm")

(define (father-daughter)
  (let ((Moore 'Mary)
        (Barnacle 'Melissa)
        (Hall (amb 'Lorna 'Gabrielle))
        (Downing (amb 'Lorna 'Rosalind 'Gabrielle))
        (Parker (amb 'Lorna 'Rosalind)))
    (require (cond ((eq? Hall 'Gabrielle) (eq? 'Rosalind Parker))
                   ((eq? Downing 'Gabrielle) (eq? 'Melissa Parker))
                   (else false)))
    (require (distinct? (list Hall Downing Parker)))
    (list (list 'Barnacle Barnacle)
          (list 'Moore Moore)
          (list 'Hall Hall)
          (list 'Downing Downing)
          (list 'Parker Parker))))


