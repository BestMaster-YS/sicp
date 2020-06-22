#lang Racket

;(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
(require "frame.rkt")
(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))

(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))

;; 习题2.48
(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define s1 (make-segment (make-vect 1 1)
                         (make-vect 2 2)))

(define s2 (make-segment (make-vect 2 2)
                         (make-vect 2 1)))


(define (vector-to-posn v)
  (make-posn (car v) (cdr v)))

(define (drawline v1 v2)
  (line (vector-to-posn v1) (vector-to-posn v2)))


(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (drawline
        ((frame-coorp-map frame) (start-segment segment))
        ((frame-coorp-map frame) (end-segment segment))))
     segment-list)))


;; 习题2.49
;(a) 
(define top-left (make-vect 0.0 1.0))

(define top-right (make-vect 1.0 1.0))

(define bottom-left (make-vect 0.0 0.0))

(define bottom-right (make-vect 1.0 0.0))

(define top (make-segment top-left top-right))

(define left (make-segment top-left bottom-left))

(define right (make-segment top-right bottom-right))

(define bottom (make-segment bottom-left bottom-right))

((segments->painter (list top bottom left right))
 (make-frame (make-vect 8 8) (make-vect 50 10) (make-vect 10 120)))

;(b)

(define tl-br (make-segment top-left bottom-right))
(define tr-bl (make-segment top-right bottom-left))
((segments->painter (list tl-br tr-bl))
 (make-frame (make-vect 8 8) (make-vect 50 10) (make-vect 10 120)))


;(a)

(define top-m (scale-vect 0.5 (add-vect top-left top-right)))
(define left-m (scale-vect 0.5 (add-vect top-left bottom-left)))
(define bottom-m (scale-vect 0.5 (add-vect bottom-left bottom-right)))
(define right-m (scale-vect 0.5 (add-vect bottom-right top-right)))

(define lt (make-segment left-m top-m))
(define lb (make-segment left-m bottom-m))
(define rb (make-segment right-m bottom-m))
(define rt (make-segment right-m top-m))

((segments->painter (list lt rb rt lb))
 (make-frame (make-vect 8 8) (make-vect 50 10) (make-vect 10 120)))







