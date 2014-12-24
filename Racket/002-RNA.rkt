; Project Rosalind, problem 2
; http://rosalind.info/problems/rna/
#lang racket
(define sample "GATGGAACTTGACTACGTAAATT")
(define expect "GAUGGAACUUGACUACGUAAAUU")
(define (transcribe dna)
  (list->string
  (map (lambda (c)
         (if (char=? c #\T)
             #\U
             c))
       (string->list dna))))