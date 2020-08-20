(define (front-ptr q) (car q))
(define (rear-ptr q) (cdr q))
(define (set-front-ptr! q item)
  (set-car! q item))
(define (set-rear-ptr! q item)
  (set-cdr! q item))

(define (empty-queue? q)
  (null? (front-ptr q)))

(define (make-queue) (cons '() '()))

(define (front-queue q)
  (if (empty-queue? q)
      (error "FRONT calls with empty queue" q)
      (car (front-ptr q))))

(define (insert-queue q item)
  (let ((new-pair (cons item '())))
    (if (empty-queue? q)
        (begin (set-front-ptr! queue new-pair)
               (set-rear-ptr! queue new-pair)
               queue)
        (begin (set-cdr! (rear-ptr queue) new-pair)
               (set-rear-ptr! queue new-pair)
               queue))))

(define (delete-queue q)
  (if (empty-queue? q)
      (error "DELETE-QUEUE calls with empty queue" queue)
      (begin (set-front-ptr! q (cdr (front-ptr q)))
             queue)))


;; OKR  O₁ 快速融入中台people前端团队，并成为其中坚力量
;;         k₁ 成为项目的 owner
;;         k₂ 
;;      O₂ 迭代
