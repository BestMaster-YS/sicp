(define (assoc key records)
  (cond ((null? records) false)
        ((equal? (caar records) key) (car records))
        (else
         (assoc key (cdr records)))))

;; one-dimensional tables
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value)
                        (cdr table))))
    'ok))

(define (make-table) (list '*table*))

;; two-dimensional tables

(define (lookup2d! key1 key2 table)
  (let ((subtable (assoc key1 (cdr table))))
    (if subtable
        (let ((record (assoc key2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert2d! key1 key2 value table)
  (let ((subtable (assoc key1 (cdr table))))
    (if subtable
        (insert! key2 value subtable)
        (set-cdr! table
                  (cons (list key1 (cons key2 value))
                        (cdr table))))
    'ok))



(define (make-general-table)
  (let ((table (list '*general-table*)))
    ;
    (define (lookup-general table keys)
      (cond ((null? keys) (error "LOOKUP don't have keys"))
            ((= (length keys) 1)
             (let ((record (assoc (car keys) (cdr table))))
               (if record
                   (cdr record)
                   false)))
            (else
             (let ((subtable (assoc (car keys) (cdr table))))
               (if subtable
                   (lookup-general subtable (cdr keys))
                   #f)))))
    (define (insert-general table value keys)
      (cond ((= (length keys) 0)
             (error "INSER don't calls with keys"))
            ((= (length keys) 1)
             (let ((record (assoc (car keys) (cdr table))))
               (if record
                   (set-cdr! record value)
                   (set-cdr! table
                             (cons (cons (car keys) value)
                                   (cdr table))))
               table))
            (else
             ;; need verify that subtable is exist
             (let ((subtable (assoc (car keys) (cdr table))))
               (if subtable
                   ;; subtable exist -> recursive
                   (insert-general subtable value (cdr keys))
                   ;; subtable don't exist -> create table -> recursive
                   (begin
                     ;; create new-subtable and insert next-table into new-subtable finally insert new-subtable into table
                     (set-cdr! table
                               (cons (insert-general (list (car keys)) value (cdr keys)) (cdr table)))
                     table))))))
    (define (dispatch op)
      (cond ((eq? op 'lookup) (lambda (keys) (lookup-general table keys)))
            ((eq? op 'insert) (lambda (value keys) (insert-general table value keys)))
            (else
             (error "Unknown op -- MAKE_GENERAL_TABLE" op))))
    dispatch))

(define operate-table (make-general-table))
(define (put value . keys) ((operate-table 'insert) value keys))
(define (get . keys) ((operate-table 'lookup) keys))



(put 1 'rational 'a 'b)

(put 2 'rational 'a)

(put 3 'scheme-number 'b)

(get 'rational 'a 'b)
(get 'rational 'a)

