(define (cond-recipient? clause)
  (and (= (length clause) 3)
       (eq? (cadr clause) '=>)))

(define (cond-test clause) (car clause))

(define (cond-recipient clause) (caddr clause))

(define (cond-actions clause)
  (let ((clause-car (car clause))))
  (if (cond-recipient? clause)
      (list ((cond-recipient clause) (clause-car)))
      (cdr clause)))

(define (expand-clauses clauses)
  (if (null? clause)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clauses? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions frist))
                     (expand-clauses rest))))))


