(load "util.scm")

;; 八皇后

;; 迭代 每添加新一列(list 1 2 3 4 5 6 7 8)，对每个位置去匹配原有的解，若通过则添加保留，

(define (queens board-size)
  (define (queens-cols k result)
    (if (> k board-size)
	result
	(queens-cols (+ k 1)
		     ;; 进行添加新列并进行过滤
		     (accmulate (lambda (row new-solution)
				  ;; 对新列行数进行遍历判断
				  (accmulate (lambda (old-solution new)
					       (if (safe-solution? row k old-solution)
						   (append new (list (append old-solution (list row))))
						   new))
					     new-solution
					     result))
			        '()
				(enumerate-interval 1 board-size)))))
  (queens-cols 2 (map (lambda (x) (list x))
		      (enumerate-interval 1 board-size))))


(define (safe-solution? row col solution)
  (define (safe-iter i solve)
    (cond ((null? solve) #t)
	  ((= row (car solve)) #f)
	  ((= (- col i) (abs (- row (car solve)))) #f)
	  (else
	   (safe-iter (+ 1 i) (cdr solve)))))
  (safe-iter 1 solution))

(queens 8)

(define (queens board-size)
  (define (queens-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (position) (safe? k position))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queens-cols (- k 1))))))
  (queens-cols board-size))

(define empty-board '())

(define (adjoin-position row col rest-of-queens) (append rest-of-queens (list row)))

(define (safe? k position)
  (if (= k 1)
      #t
      (safe-solution? (car (reverse position)) k (reverse (cdr (reverse position))))))

(queens 8)

