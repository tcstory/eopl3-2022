#lang eopl

(define (product sos1 sos2)
  (flatten (map (lambda (x)
                  (map (lambda (y)
                         (list x y)) sos2))
                sos1)))

(define (flatten sos)
  (if (null? sos)
      `()
      (append (car sos) (flatten (cdr sos)))))

;  (product '(a b c) '(x y))
; (((a x) (a y)) ((b x) (b y)) ((c x) (c y)))