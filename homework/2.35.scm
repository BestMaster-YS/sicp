(load "util.scm")

(define (count-leaves t)
  (accmulate (lambda (cur rest)
	       (if (pair? cur)
		   (+ (count-leaves cur) rest)
		   (+ 1 rest)))
	     0
	     t))

(count-leaves (list 1 (list 2 (list 3 4) 5) (list 6 7)))




