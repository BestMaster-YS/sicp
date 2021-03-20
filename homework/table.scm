;; one-dimensional tables
(define (lookup key table)
  (let ((record (assoc key (cdr tabel))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? (caar records) key) (car records))
        (else
         (assoc key (cdr records)))))


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
              '*unassigned*))
        '*unassigned*)))

(define (insert2d! key1 key2 value table)
  (let ((subtable (assoc key1 (cdr table))))
    (if subtable
        (insert! key2 value subtable)
        (set-cdr! table
                  (cons (list key1 (cons key2 value))
                        (cdr table))))
    'ok))

;; creating local state

(define (make-table-local)
  (let ((local-table (list '*local-table*)))
    (define (lookup key1 key2)
      (let ((subtable (assoc key1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key2 (cdr subtable))))
              (if record
                  (cdr record)
                  '*unassigned*))
            '*unassigned*)))

    (define (insert key1 key2 value)
      (let ((subtable (assoc key1 (cdr local-table))))
        (if subtable
            (insert! key2 value subtable)
            (set-cdr! local-table
                      (cons (list key1 (cons key2 value))
                            (cdr local-table))))
        'ok))
    (define (dispatch op)
      (cond ((eq? op 'lookup) lookup)
            ((eq? op 'insert) insert)
            (else
             (error "Unknown operation: TABLE" op))))
    dispatch))

(define (put table . keys) (apply (table 'insert) keys))
(define (get table . keys) (apply (table 'lookup) keys))


