#lang eopl

(define (invert lst)
  (map swap lst))

; 不太优雅的方式
(define (swap lst)
  (cons (cadr lst) (cons (car lst) `())))
  

(invert `((a 1) (a 2) (b 1) (b 2)))
