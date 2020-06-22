#lang Racket

(require "frame.rkt")
(require "painter.rkt")

;; frame-coorp-map 将向量 v(x,y) 映射到框架上 frame
;; origin(frame) + x*edge1(frame) + y*edge2(frame)

;; 对于画家的操作都基于 transform-painter 而它又是基于对框架的参数的改变得到新的框架再调用 painter
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    ;; m :: vect -> vect
    (let ((m (frame-coorp-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))
;; 以基准框架为例 (0,0) (1,0) (0,1)

(define (flip-vert painter)
  (transform-painter painter
                     ;; o = o + e2 = (0,1)
                     ;; e1 = o + e1 + e2 - o + e2 = e1 = (1.0)
                     ;; e2 = o - o - e2 = -e2 = (0,-1)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

;; 将图像缩小到框架的右上四分之一区域
(define (shrink-to-up-right painter)
  (transform-painter painter
                     ;; o = o + 0.5e1 + 0.5e2 = 
                     ;; e1 = o + e1 + 0.5e2 - 0.5e1 - 0.5e2 = 0.5e1
                     ;; e2 = 0.5e2
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))


;; 逆时针旋转90度
(define (rotate90 painter)
  (transform-painter painter
                     ;; o = o + e1
                     ;; e1 = o + e1 + e2 - o -e1 = e2
                     ;; e2 = o - o - e1 = -e1
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))












