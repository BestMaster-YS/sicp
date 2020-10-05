;(load "util.scm")
;; 常规方法计算谜题问题

(define init
  (list (list 1 2 3 4) ;; baker
        (list 2 3 4 5) ;; cooper
        (list 2 3 4) ;; fletcher
        (list 3 4 5) ;; miller
        (list 1 2 3 4 5))) ;; smith

(define (flatmap proc list)
  (if (null? list)
      '()
      (let ((result (proc (car list)))
            (rest (flatmap proc (cdr list))))
        (if (pair? result)
            (append result rest)
            (cons result rest)))))

;;  全排列
(define (permutation lists)
  (if (null? lists)
      '(())
      (flatmap (lambda (x)
                 (map (lambda (y) (cons x y))
                      (permutation (cdr lists))))
               (car lists))))

(define (member? a lst)
  (cond ((null? lst) #f)
        ((eq? (car lst) a) #t)
        (else (member? a (cdr lst)))))

(define (distinct? list)
  (cond
   ((null? list) #t)
   ((member? (car list) (cdr list)) #f)
   (else
    (distinct? (cdr list)))))

;; 限制
(define (restrictions l)
  (apply (lambda (baker cooper fletcher miller smith)
           (and (> miller cooper)
                (not (= (abs (- smith fletcher)) 1))
                (not (= (abs (- fletcher cooper)) 1))
                (distinct? (list baker cooper fletcher miller smith))))
         l))

(define (multiple-dwelling-without-amb)
  (filter restrictions (permutation init)))

(multiple-dwelling-without-amb)
