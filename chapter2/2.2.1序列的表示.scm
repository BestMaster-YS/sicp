(define one-through-four (list 1 2 3 4))

one-through-four

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1)))
)

(define (length itmes)
  (if (null? items)
      0
      (+ (length (cdr items)) 1))
)

(define (append list1 list2)
	(if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2)))
)

(list-ref one-through-four 2)

(define nil '())

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor)))
)

(scale-list (list 1 2 3) 2)

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items))))
)
