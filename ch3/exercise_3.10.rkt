#lang eopl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   sacn&parse                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define the-grammar
  '((program (expression) a-program)

    (expression (number) const-exp)

    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)


    (expression
     ("+" "(" expression "," expression ")")
     add-exp)


    (expression
     ("*" "(" expression "," expression ")")
     mul-exp)

    (expression
     ("/" "(" expression "," expression ")")
     div-exp)

    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression
     ("minus" "(" expression ")")
     minus-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression (identifier) var-exp)

    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)

    (expression
     ("emptylist")
     empty-exp)
    (expression
     ("null?" "(" expression ")")
     null?-exp)
    (expression
     ("cons" "(" expression "," expression ")")
     cons-exp)
    (expression
     ("car" "(" expression ")")
     car-exp)
    (expression
     ("cdr" "(" expression ")")
     cdr-exp)

    (expression ("list" "(" (separated-list expression ",") ")" ) list-exp)

    ))

(define identifier? symbol?)

(define-datatype program program?
  (a-program
   (exp1 expression?)))

(define-datatype expression expression?
  (const-exp
   (num number?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (add-exp
   (exp1 expression?)
   (exp2 expression?))
  (mul-exp
   (exp1 expression?)
   (exp2 expression?))
  (div-exp
   (exp1 expression?)
   (exp2 expression?))
  (zero?-exp
   (exp1 expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp
   (var identifier?))
  (let-exp
   (var identifier?)
   (exp1 expression?)
   (body expression?))
  (minus-exp
   (exp1 expression?))

  (empty-exp)
  (null?-exp
   (exp1 expression?))
  (cons-exp
   (exp1 expression?)
   (exp2 expression?))
  (car-exp
   (exp1 expression?))
  (cdr-exp
   (exp1 expression?))
  (list-exp
   (args (list-of expression?)))

  )


(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (empty-val)
  (cons-val
   (a expval?)
   (b expval?))
  )

(define (expval->num val)
  (cases expval val
         (num-val (num) num)
         (else (report-expval-extractor-error 'num val))))

(define (expval->bool val)
  (cases expval val
         (bool-val (bool) bool)
         (else (report-expval-extractor-error 'bool val))))

(define (expval->cons val)
  (cases expval val
         (cons-val (a b) (cons a (cons b `())))
         (empty-val () `())
         (else (report-expval-extractor-error 'cons val))))


(define report-expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

(define (list-val-handler args)
  (if (null? args)
      (empty-val)
      (cons-val
       (car args)
       (list-val-handler (cdr args)))))


(define (run string)
  (value-of-program (scan&parse string)))

(define (value-of-program pgm)
  (cases program pgm
         (a-program (exp1) (value-of exp1 (init-env)))))

(define (value-of exp env)
  (cases expression exp
         (const-exp (num) (num-val num))
         (var-exp (var) (apply-env env var))
         (diff-exp (exp1 exp2)
                   (num-val
                    (-
                     (expval->num (value-of exp1 env))
                     (expval->num (value-of exp2 env)))))
         (add-exp (exp1 exp2)
                  (num-val
                   (+
                    (expval->num (value-of exp1 env))
                    (expval->num (value-of exp2 env)))))
         (mul-exp (exp1 exp2)
                  (num-val
                   (*
                    (expval->num (value-of exp1 env))
                    (expval->num (value-of exp2 env)))))
         (div-exp (exp1 exp2)
                  (num-val
                   (/
                    (expval->num (value-of exp1 env))
                    (expval->num (value-of exp2 env)))))
         (zero?-exp (exp1) (bool-val (zero? (expval->num(value-of exp1 env)))))
         (if-exp (exp1 exp2 exp3)
                 (if (expval->bool (value-of exp1 env))
                     (value-of exp2 env)
                     (value-of exp3 env)))
         (let-exp (var exp1 body)
                  (value-of
                   body
                   (extend-env var (value-of exp1 env) env)))
         (minus-exp (exp1)
                    (num-val (- 0 (expval->num (value-of exp1 env)))))

         (empty-exp () (empty-val))
         (null?-exp (exp1)
                    (bool-val
                     (null?
                      (expval->cons (value-of exp1 env))))
                    )
         (cons-exp (exp1 exp2)
                   (cons-val
                    (value-of exp1 env)
                    (value-of exp2 env)
                    ))
         (car-exp (exp1)
                  (car
                   (expval->cons (value-of exp1 env))))

         (cdr-exp (exp1)
                  (cadr
                   (expval->cons (value-of exp1 env))))
         (list-exp (args)
                   (list-val-handler (map (lambda (arg)
                                            (value-of arg env)) args)))
         ))


(define (empty-env)
  (lambda (search-var)
    (report-no-binding-found search-var)))


(define (extend-env saved-var saved-val saved-env)
  (lambda (search-var)
    (if (eqv? search-var saved-var)
        saved-val
        (apply-env saved-env search-var))))

(define (apply-env env search-var)
  (env search-var))


(define (report-no-binding-found search-var)
  (eopl:error `apply-env "No binding for ~s" search-var))

(define (report-invalid-env env)
  (eopl:error `apply-env "Bad environment: ~s" env))


(define (init-env)
  (extend-env
   'i (num-val 1)
   (extend-env
    'v (num-val 5)
    (extend-env
     'x (num-val 10)
     (empty-env)))))

;; code

;; (run
;;  "
;; let x = 4
;; in cons(x,
;;     cons(cons(-(x, 1),
;;             emptylist),
;;           emptylist))
;; "
;;  )

;; (run "let x = 4 in -(10, x)")

; (run "cons(1, 2)")
; (run "car (cons (1, 2))")
; (run "cdr (cons (1, 2))")
; (run "null? (emptylist)")
; (run "null? (cons (1, 2))")
;
;
; (run "list(1, 2, 3)")
;(run "car(cdr(list(1, 2, 3)))")
;(run "let x = 4 in list(x, -(x,1), -(x,3))")
