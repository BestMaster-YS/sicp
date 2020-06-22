(define (equal? v1 v2)
  (cond ((and (pair? v1) (pair? v2))
	 ;; 都是表
	 (cond ((and (null? v1) (null? v2)) #t)
	       ((and (not (null? v1)) (not (null? v2)))
		;; 都不为空表
		(and (equal? (car v1) (car v2))
		     (equal? (cdr v2) (cdr v2)))
		(else
		 #f))))
	; 都不是表
	((and (not (pair? v1)) (not (pair? v2)))
	 (eq? v1 v2))
	(else
	 #f)))

(equal? '(this is a list) '(this (is a) list))
(equal? '(this is a list) '(this is a list))

