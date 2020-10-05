;; 逻辑谜题

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (flectcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper flectcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= flectcher 5)))
    (require (not (= flectcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith flectcher)) 1)))
    (require (not (= (abs (- flectcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'flectcher flectcher)
          (list 'miller miller)
          (list 'smith smith))))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else
         (distinct? (cdr items)))))

;; 自然语法的语法分析

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))

(define *unparsed* '())

;; 句子由名词短语和动词短语组成
(define (parse-sentence)
  (list 'noun-phrase
        (parse-noun-phrase)
        (parse-verb-phrase)))

(define (parse-noun-phrase)
  (list 'noun-phrase
        (parse-word acticles)
        (parse-word nouns)))

(define (parse-preposition-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

;; 动词短语可以是动词或者是动词短语+介词短语
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-preposition-phrase)))))
  (maybe-extend (parse-word verbs)))


(define (parse-woed word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car word-list)))
    (set! *unparsed* (cdr word-list))
    (list (car word-list) found-word)))

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word acticles)
        (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-preposition-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))






