#lang eopl

(define (down lst)
  (map (lambda (x)
         (list x))
  lst))

(down `(1 2 3))
(down `((a) (fine) (idea)))
(down `(a (more (complicated)) object))