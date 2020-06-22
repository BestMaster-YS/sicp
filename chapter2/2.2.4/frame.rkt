#lang Racket

;; 框架
;(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(provide make-vect xcor-vect ycor-vect frame-coorp-map add-vect sub-vect scale-vect make-frame origin-frame edge1-frame edge2-frame)


(define (frame-coorp-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))


;; 习题2.46
(define (make-vect x y)
  (cons x y))

(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))


;; 习题2.47
;; 脚向量都是基于基准向量，不能视为基于原点（0，0），所以在
;; frame-coorp-map 中需要加上 origin-frame
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))


(define (origin-frame frame)
  (car frame))


(define (edge1-frame frame) (cadr frame))

(define (edge2-frame frame) (caddr frame))

;(define (make-frame origin edge1 edge2)
;  (cons origin (cons edge1 edge2)))




