;; A deque("double-end queue") is a sequence in which items can be inserted and deleted at either the front or the the rear.
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (make-deque) (cons '() '()))
(define (set-front-ptr! q item)
  (set-car! q item))
(define (set-rear-ptr! q item)
  (set-cdr! q item))


(define (empty-deque? queue) (null? (front-ptr queue)))
(define (front-deque queue) (car (front-ptr queue)))
(define (rear-deque queue) (car (rear-ptr queue)))

(define (insert-front-deque! deque item)
  (if (empty-deque? deque)
      (let ((new-pair (cons item '())))
        (begin (set-front-ptr! deq new-pair)
               (set-rear-ptr! deq new-pair)
               new-pair) )
      (begin (set-front-ptr! deque (cons item (front-ptr deque)))
             (front-ptr deque))))

(define (insert-rear-deque! queue item)
  (let ((new-pair (cons item '())))
    (if (empty-deque? queue)
        (begin (set-front-ptr! queue new-pair)
               (set-rear-ptr! queue new-pair)
               queue)
        (begin (set-cdr! (rear-ptr queue) new-pair)
               (set-rear-ptr! queue new-pair)
               queue))))


(define (delete-front-queue! q)
  (if (empty-deque? q)
      (error "DELETE-QUEUE calls with empty queue" q)
      (begin (set-front-ptr! q (cdr (front-ptr q)))
             q)))

;; delete the item of deque O(n)
(define (delete-rear-queue! q)
  (if (empty-deque? q)
      (error "DELETE-QUEUE calls with empty deque" q)
      (begin (set-front-ptr! q (delete-last (front-ptr q)))
             (set-rear-ptr! q (last-pair (front-ptr q)))
             (front-ptr q))))

(define (delete-last seq)
  (cond ((null? seq) (error "LAST_PREV dont't hvae more items " seq))
        ((= (length seq) 1) '())
        (else
         (cons (car seq) (delete-last seq)))))


;; test


(define deq (make-deque))

(insert-front-deque! deq 1)
(insert-rear-deque! deq 2)

(delete-rear-queue! deq)
