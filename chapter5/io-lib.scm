(define (read-file file-name)
  (let ((p (open-input-file file-name)))
    (let loop((ls1 '()) (c (read-char p)))
      (if (eof-object? c)
          (begin
            (close-input-port p)
            (list->string (reverse ls1)))
          (loop (cons c ls1) (read-char p))))))

(define (write-file filename content)
  (let ((port (open-output-file filename)))
    (display content port)
    (close-output-port port)))

(define target (read-file "compile.txt"))

(define (string-replace target s1 s2)
  (define len1 (string-length target))
  (define len2 (string-length s1))
  (define (iter idx res)
    (cond ((> (+ idx len2) len1)
           (string-append res (substring target idx len1)))
          ((string=? s1 (substring target idx (+ idx len2)))
           (iter (+ idx len2) (string-append res s2)))
          (else
           (iter (+ idx 1) (string-append res (substring target idx (+ idx 1)))))))
  (iter 0 ""))



