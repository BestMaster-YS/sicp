
;;
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          ;; 因为 outranked-by 在 and 分支中先执行，而且 ?middle-manager 的意义与 ?staff-person 相同
          ;; 只能继续寻找下一个 ?middle-manager ，陷入死循环
          (and (outranked-by ?middle-manager ?boss)
               (supervisor ?staff-person ?middle-manager))))




