(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))


(data-paths
 (register
  ((name product)
   (buttons ((name product*counter) (source (operation *)))))
  ((name countter)
   (buttons ((name counter+1) (source (operation +)))))
  ((name n)
   (buttons ((name n-init) (source (constant n))))))
 (operation
  ((name *)
   (inputs (register product) (register counter)))
  ((name +)
   (inputs (register counter) (constant 1)))))

(controller
 test-counter
 (test (op >) (reg counter) (reg n))
 (branch (label fact-done))
 (assign product (op *) (reg product) (reg counter))
 (assign counter (op +) (reg counter) (const 1))
 (goto (label test-counter))
 fact-done)




