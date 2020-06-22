(load "2.2.scm")
(load "util.scm")

;; 在此题中矩形的选择函数为中层，周长和面积公式为上层程序使用矩形公共的选择函数
;; 矩形的构成则为底层数据抽象
;; 此题选择矩形的选择函数为返回一个线段，也可以实现为返回长度和宽度

;; 第一种实现方法，接受高宽两个线段
(define (rectangle height width)
  (cons height width)
)

; 第二种实现方法，接受一个点，宽 高
(define (rectangle p width height)
  (cons (make-segment p (make-point (+ (x-point p) width)
                                    (y-point p)))
        (make-segment p (make-point (x-point p)
                                    (+ (y-point p) height))))
)


(define (h-rect rect) (car rect))
(define (w-rect rect) (cdr rect))

;; 计算线段长度
(define (segment-length segment)
  (sqrt (+ (square (- (x-point (end-segment segment))
                      (x-point (start-segment segment))))
           (square (- (y-point (end-segment segment))
                      (y-point (start-segment segment))))))
)

(define (area-rectangle rect)
  (* (segment-length (h-rect rect))
     (segment-length (w-rect rect)))
)

(define (perimeter-rectangle rect)
  (* (+ (segment-length (h-rect rect))
        (segment-length (w-rect rect))) 2)
)
