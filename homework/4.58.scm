
(rule (bigname ?person)
      (and (supervisor ?person ?super)
           (job ?person (?department1 . ?x))
           (job ?super (?department2 . ?y))
           (not (same ?department1 ?department2))))






