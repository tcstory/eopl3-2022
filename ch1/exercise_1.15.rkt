#lang eopl

(define (duple x n)
  (if (eqv? x 0)
      `()
      (cons n (duple (- x 1) n))))