;; a)
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

;; explicit-control-evalutor
;; n               total             max
;; 1                16                8
;; 2                48                13
;; 3                80                18
;; 4                112               23
;; 5                144               28

;; evaluator      32n - 16          5n + 3



;; compiler
;; 1                7                 3
;; 2               13                 5
;; 3               19                 8
;; 4               25                 11
;; 5               31                 14

;; compiler      6n + 1              3n - 1

;; special-machine
;; 1                0                 0
;; 2                2                 2
;; 3                4                 4
;; 4                6                 6
;; 5                8                 8

;; manual        2n - 2             2n - 2

;; b)
;; 提高性能，也就是减少指令执行，减少堆栈的使用，使用开放式代码优化，对于开放式代码
;; 对于 end-with-linkage



