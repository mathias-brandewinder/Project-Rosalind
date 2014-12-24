; Project Rosalind, problem 3
; http://rosalind.info/problems/revc/
#lang racket
(define (comp c)
  (match c
    [#\A #\T]
    [#\T #\A]
    [#\C #\G]
    [#\G #\C]))
(define (complement dna)
   (list->string
    (reverse
     (map (lambda (c)
       (comp c)) 
       (string->list dna)))))
(define sample "AAAACCCGGT")
(define expect "ACCGGGTTTT")
(string=? expect (complement sample))