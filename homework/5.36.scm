;; according to the execise 5.35, we found that to compiled the expression (+ x (g (+ x 2))), it will
;; first evaluate (g (+ x 2)), then construct arglist (x (g (+ x 2))), finally apply '+' operator.

;; in our compiler,  when we compile the application, we will construct the arguments list, in order last-to-first.



