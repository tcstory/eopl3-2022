#lang eopl


(define (count s1 s2 n)
  (if (eqv? s1 s2)
      (+ n 1)
      n))

(define (loop s slist n)
  (if (null? slist)
      n
      (let ((cur (car slist)))
        (if (list? cur)
            (loop s (cdr slist) (loop s cur n))
            (loop s (cdr slist) (count s cur n))))))
      

(define (count-occurrence s slist)
  (loop s slist 0))


; (count-occurrence 'x '((f x) y (((x z) x))))
; (count-occurrence 'x '(y (x)))
; (count-occurrence 'x '(y x z))
; (count-occurrence 'x '((f x) y (((x z) () x))))
;  (count-occurrence 'w '((f x) y (((x z ) x))))
