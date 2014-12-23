; Project Rosalind, problem 1
; http://rosalind.info/problems/dna/
#lang racket
(define (list-sum lst)
    (foldl (lambda (elem acc)
             (+ acc elem))
           0
           lst))
(define (count-char c lst)
  (list-sum(map (lambda (x)
         (if (char=? x c) 
             1 
             0)) lst)))
(define strands (string->list "ACGT"))
(define (rosalind data)
  (map (lambda (c)
         (count-char c (string->list data))) strands))
(define sample "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC")
(rosalind sample)