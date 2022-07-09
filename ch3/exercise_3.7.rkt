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
   (exp1 expression?)))


(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?)))

(define (expval->num val)
  (cases expval val
    (num-val (num) num)
    (else (report-expval-extractor-error 'num val))))

(define (expval->bool val)
  (cases expval val
    (bool-val (bool) bool)
    (else (report-expval-extractor-error 'num val))))


(define report-expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))


(define (run string)
  (value-of-program (scan&parse string)))

(define (value-of-program pgm)
  (cases program pgm
    (a-program (exp1) (value-of exp1 (init-env)))))

(define (value-of exp env)
  (cases expression exp
         (const-exp (num) (num-val num))
         (var-exp (var) (apply-env var env))
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
         ))


(define (empty-env)
  (lambda (search-var)
    (report-no-binding-found search-var)))


(define (extend-env saved-var saved-val saved-env)
  (lambda (search-var)
    (if (eqv? search-var saved-var)
        saved-val
        (apply saved-env search-var))))

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

;; (run "minus(-(minus(5), 9))")
;; (run  "if zero?(-(11,11)) then minus(3) else 4")
;; (run "+(5, 10)")
;; (run "/(1, 3)")
;; (run "*(1, 10)")
;; (run "*(3, /(1, 3))")
