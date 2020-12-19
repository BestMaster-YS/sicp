

(rule (same-job ?p1 ?p2)
      (and (job ?p1 ?x)
           (job ?p2 ?x)))

(rule (replace ?p1 ?p2)
      (and (not (same ?p1 ?p2))
           (or (same-job ?p1 ?p2)
               (and (job ?p1 ?job1)
                    (job ?p2 ?job2)
                    (can-do-job ?job1 ?job2)))))

;; a)
(replace ?p1 (Fect Cy D))

;; b)
(and (salary ?person1 ?amount1)
     (salary ?person2 ?amount2)
     (replace ?person1 ?person2)
     (list-value > ?amount2 ?amount1))



