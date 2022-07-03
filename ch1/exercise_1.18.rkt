#lang eopl


(define (swapper s1 s2 slist)
  (map (lambda (x)
         (cond
           [(null? x) `()]
           [(list? x) (swapper s1 s2 x)]
           [else (swap-item s1 s2 x)]))
  slist))

(define (swap-item s1 s2 x)
  (cond
    [(eqv? s1 x) s2]
    [(eqv? s2 x) s1]
    [else x]))

; (swapper 'a 'd '(a b c d))
; (swapper 'a 'd '(a d () c d))
; (swapper 'x 'y '((x) y (z (x))))