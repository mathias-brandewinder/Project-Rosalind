; http://rosalind.info/problems/fib/
#lang racket
; compute next population pair
; pop = old,new
(define (next pop growth)
  (match pop
    [(cons old new) (cons (+ new old) (* old growth))]))

(define (total pop)
  (match pop
    [(cons old new) (+ old new)]))

(define (grow pop iters growth)
  (if (> iters 1)
      (grow (next pop growth) (- iters 1) growth)
      (total pop)))

; check against example
(equal? (grow (cons 0 1) 5 3) 19)