;; a)
(and (supervisor ?person (Bitdiddle ben))
     (address ?person ?where))

;; b)
(and (salary (Bitdiddle ben) ?amount1)
     (salary ?person ?amount)
     (list-value < ?amount ?amount1))

;; c)

(and (supervisor ?person ?super)
     (not (job ?super (computer . ?x)))
     (job ?person ?job))



