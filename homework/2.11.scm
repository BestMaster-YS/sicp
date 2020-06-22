(load "2.10.scm")

(define (mul-interval a b)
  (let ((pos? (lambda (x) (>= x 0)))
        (neg? (lambda (x) (<  x 0)))
        (al (lower-bound a))
        (au (upper-bound a))
        (bl (lower-bound b))
        (bu (lower-bound b)))
    (cond ((and (pos? al) (pos? bl)) ;; 都大于0
           (make-interval (* al bl) (* au bu)))
          ((and (neg? au) (neg? bu))  ;; 都小于0
           (make-interval (* au bu) (* al bl)))
          ((and (pos? al) (neg? bu)) ;; 正区间 * 负区间
           (make-interval (* au bl) (* al bu)))
          ((and (neg? au) (pos? bl)) ;;负区间 * 正区间
           (make-interval (* bu al) (* au bl)))
          ((and (pos? au) (neg? al) (pos? bl)) ;; 横跨 * 正
           (make-interval (* bu al) (* au bu)))
          ((and (pos? au) (neg? al) (neg? bu)) ;; 横跨 * 负
           (make-interval (* au bl) (* al bl)))
          ((and (pos? al) (neg? bl) (pos? bu)) ;; 正 * 横跨
           (make-interval (* au bl) (* au bu)))
          ((and (neg? au) (neg? bl) (pos? bu)) ;; 负 * 横跨
           (make-interval (* al bl) (* al bu)))
          (else ;; 横跨 * 横跨
            (make-interval (min (* al bu) (* au bl))
                           (max (* al bl) (* au bu)))))
  )
)

(define (make-center-width c w)
  (make-interval (- c w) (+ c w))
)

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2)
)

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2)
)