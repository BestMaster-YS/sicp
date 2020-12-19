
(meeting accounting (Monday 9am))
(meeting administration (Monday 10am))
(meeting computer (Wednesday 3pm))
(meeting administration (Firday 1pm))
(meeting whole-company (Wednesday 4pm))

;; a)
(and (meeting ?department (?week . ?rest))
     (same ?department computer)
     (same week Firday))


;; b)
(rule (meeting-time ?person (?day ?time))
      (or (and (job ?person (?department . ?rest))
               (meeting ?department (?day1 ?time1))
               (list-value > ?time1 ?time)
               (same ?day1 ?day))
          (and (meeting whole-company ?day-and-time))))


;; c)
(meeting-time (Hacker Alyssa P) (Wednesday 10am))


