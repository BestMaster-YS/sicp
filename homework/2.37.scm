;; 矩阵运算

(load "util.scm")

;; 点积

;; 返回数
(define (dot-product v w)
  (accmulate + 0 (accmulate-n * 1 (list v w))))

(dot-product (list 1 2 3 4) (list 1 2 3 4))


;; 线性映射 t(t1 t2 t3 ...)
(define (martix-*-vector m v)
  (map (lambda (row) (dot-product v row))  m))

(define martix1 (list (list 1 2 1) (list 1 1 2) (list 2 1 1)))
(define martix2 (list (list 1 1 1) (list 2 1 2) (list 1 1 1)))
(define col (list 3 2 1))

(martix-*-vector martix1 col)

;; 转置 
(define (transpose mat)
  (accmulate-n cons '()  mat))

(transpose martix1)

;; 矩阵乘积
(define (martix-*-martix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (map (lambda (col) (dot-product row col)) cols))  m)))

(martix-*-martix martix1 martix2)
