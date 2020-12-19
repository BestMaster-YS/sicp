
(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))))

(rule (same ?person ?person))

;; 出现两对相同的情况

(rule (lives-near-unique ?person1 ?person2)
      (and (lives-near ?person1 ?person2)
           (not (lives-near ?person2 ?person1))))


