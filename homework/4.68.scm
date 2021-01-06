(rule (same ?x ?x))

(rule (reverse () ()))
(rule (reverse ?x ?y)
      (and (append-to-from (?first) ?rest ?x)
           (append-to-from ?rev-rest (?first) ?y)
           (reverse ?rev-rest ?rest)))
