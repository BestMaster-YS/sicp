#lang Racket

(require "picture.rkt")

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))


(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))


(define (fliped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-horiz)))
    (combine4 painter)))

(paint (fliped-pairs einstein))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))


(paint (square-limit einstein 4))



(define (split out-op in-op)
  (define (split-iter painter n)
    (if (= n 0)
        painter
        (let ((smaller (split-iter painter (- n 1))))
          (out-op painter (in-op smaller smaller)))))
  split-iter)




