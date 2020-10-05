(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

;; 可以生成所有的三元组，但是效率不高，三个无限不确定计算效率不高
;; 可以设置 K 为无限制，i 以 1 - k为限制,j 为以 i - k为限制的

(define (all-pythagorean-triple)
  (let ((k (an-integer-starting-from 5)))
    (let ((i (an-integer-between 3 k)))
      (let ((j (an-integer-between i k)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))



