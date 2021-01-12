(define (add-assertion! assertion)
  (store-asserion-in-index assertion)
  ;; (cons-stream assertion THE-ASSERTIONS) 被延迟求值
  ;; 所以 set! THE-ASSERTIONS 先执行，导致当访问 (cons-stream assertion THE-ASSERTIONS) 时 THE-ASSERTIONS 指向 (cons-stream assertion THE-ASSERTIONS)，无限循环
  (set! THE-ASSERTIONS
        (cons-stream assertion THE-ASSERTIONS))
  'ok)



