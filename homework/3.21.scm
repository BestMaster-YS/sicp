(load "util.scm")

(define q1 (make-queue))

(define (print-queue queue)
  (if (empty-queue? queue)
      '()
      (front-ptr queue)))


(insert-queue q1 'a)
(print-queue q1)
(insert-queue q1 'b)
(print-queue q1)
(delete-queue q1)
(print-queue q1)
(delete-queue q1)
(print-queue q1)
(insert-queue q1 'c)
(print-queue q1)
