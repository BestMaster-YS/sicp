;; 创世纪4 数据库

(son Adam Cain)
(son Cain Enoch)
(son Enoch Irad)
(son Irad Mehujael)
(son Mehujael Mathushael)
(son Methushael Lamech)
(wife lamech Ada)
(son Ada Jabal)
(son Ada Jubal)

;; (son F S) (son G F)
;; ()

(rule (grandson ?x ?y)
      (and (son ?x ?z)
           (son ?z ?y)))

;; otherson
(rule (otherson ?x ?y)
      (and (wife ?x ?z)
           (son ?z ?y)))
           
