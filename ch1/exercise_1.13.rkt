#lang eopl

(define (nth-element lst n)
  (if (null? lst)
      (report-list-too-short n)
      (if (= n 0)
          (car lst)
          (nth-element (cdr lst) (- n 1)))))

(define (report-list-too-short n)
  (eopl:error 'nth-element
              "List too short by ~a elements." (+ n 1)))

(define (remove-first s los)
  (cond [(null? los) `()]
        [(eq? s (car los)) (cdr los)]
        [else (cons (car los) (remove-first s (cdr los)))]))


(define (remove s los)
  (cond [(null? los) `()]
        [(eq? s (car los)) (remove s (cdr los))]
        [else (cons (car los) (remove s (cdr los)))]))

(define (subset new old slist)
  (if (null? slist)
      `()
      (cons
       (subset-in-s-exp new old (car slist))
       (subset new old (cdr slist)))))

(define (subset-in-s-exp new old sexp)
  (if (symbol? sexp)
      (if (eqv? old sexp) new sexp)
      (subset new old sexp)))

(define (subset1 new old slist)
  (if (null? slist)
      `()
      (cons
       (let ((sexp (car slist)))
         (if (symbol? sexp)
             (if (eqv? old sexp) new sexp)
             (subset1 new old sexp)))
       (subset1 new old (cdr slist)))))

(define (subset2 new old slist)
  (map (lambda (x)
         (if (list? x)
             (subset2 new old x)
             (if (eqv? x old) new x)))
  slist))

