; http://rosalind.info/problems/fib/
#lang racket
; compute next population pair
(define (next pop g)
  (match pop
    [(cons curr prev) (cons (+ curr (* prev g)) curr)]))

(define (total pop)
  (match pop
    [(cons curr prev) (+ curr prev)]))

(define (grow pop rate iters)
  (if (> iters 0)
      (grow (next pop rate) rate (- iters 1))
      (total pop)))