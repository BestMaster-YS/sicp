; accmulate-n 的第三个参数是一个拥有相同长度的序列的序列，accmulate-n 是将子序列中第一个，第二个进行累积
(load "util.scm")


(define (accmulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accmulate op init (map (lambda (seq) (car seq)) seqs))
	    (accmulate-n op init (map (lambda (seq) (cdr seq)) seqs)))))

(accmulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))



