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

    (expression ("equal?" "(" expression "," expression ")") equal?-exp)
    (expression ("less?" "(" expression "," expression ")") less?-exp)
    (expression ("greater?" "(" expression "," expression ")") greater?-exp)

    (expression
     ("minus" "(" expression ")")
     minus-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression (identifier) var-exp)

    (expression
     ("let" (arbno identifier "=" expression) "in" expression)
     let-exp)


    (expression
     ("let*" (arbno identifier "=" expression) "in" expression)
     let*-exp)

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

    (expression
     ("cond" (arbno expression "==>" expression) "end")
     cond-exp
     )

    (expression
     ("unpack" (arbno identifier) "=" expression "in" expression)
     unpack-exp
     )

    (expression
     ("proc" "(" (separated-list identifier ",") ")" expression)
     proc-exp)
    (expression
     ("(" expression (arbno expression) ")")
     call-exp)

    ))

(define identifier? symbol?)
(define environment? procedure?)

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
  (equal?-exp
   (exp1 expression?)
   (exp2 expression?))
  (less?-exp
   (exp1 expression?)
   (exp2 expression?))
  (greater?-exp
   (exp1 expression?)
   (exp2 expression?))

  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp
   (var identifier?))
  (let-exp
   (vars (list-of identifier?))
   (exps (list-of expression?))
   (body expression?))
  (let*-exp
   (vars (list-of identifier?))
   (exps (list-of expression?))
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


  (cond-exp
   (conds (list-of expression?))
   (exps (list-of expression?)))

  (unpack-exp
   (vars (list-of identifier?))
   (exp1 expression?)
   (body expression?))

  (proc-exp
   (var (list-of identifier?))
   (body expression?))
  (call-exp
   (rator expression?)
   (rands (list-of expression?)))

  )

(define-datatype proc proc?
  (procedure
   (var (list-of identifier?))
   (body expression?)
   (saved-env environment?))
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
  (proc-val
   (proc proc?))
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
         (cons-val (a b) (cons a b))
         (empty-val () `())
         (else (report-expval-extractor-error 'cons val))))

(define (expval->proc val)
  (cases expval val
         (proc-val (proc) proc)
         (else (report-expval-extractor-error 'proc val))))


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

(define (cond-exp-handler conds exps env)
  (cond
    ((null? conds) (eopl:error 'cond-exp-handler "empty"))
    ((eqv? (expval->bool (value-of (car conds) env)) #t)
     (value-of (car exps) env))
    (else
     (cond-exp-handler (cdr conds) (cdr exps) env)))
  )

(define (let-exp-handler vars exps body old-env new-env)
  (if (null? vars)
      (value-of body new-env)
      (let-exp-handler
       (cdr vars)
       (cdr exps)
       body
       old-env
       (extend-env (car vars) (value-of (car exps) old-env) new-env))))


(define (let*-exp-handler vars exps body env)
  (if (null? vars)
      (value-of body env)
      (let ((new-env (extend-env (car vars) (value-of (car exps) env) env)))
        (let*-exp-handler
         (cdr vars)
         (cdr exps)
         body
         new-env
         ))))

(define (unpack-exp-handler vars exp1 body env)
  (letrec ((loop (lambda (vars vals saved-env)
              (if (null? vars)
                  saved-env
                  (loop
                   (cdr vars)
                   (expval->cons (cdr vals))
                   (extend-env (car vars) (car vals) saved-env))))))
        (value-of body (loop vars (expval->cons (value-of exp1 env)) env))))

(define (extend-env-multiple vars vals env)
  (if (null? vars)
      env
      (extend-env-multiple
       (cdr vars)
       (cdr vals)
       (extend-env (car vars) (car vals) env)))
  )


(define (apply-procedure proc1 vals)
  (cases proc proc1
        (procedure (vars body saved-env)
                   (value-of body (extend-env-multiple vars vals saved-env)))))

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
         (equal?-exp (exp1 exp2)
                     (let ((val1 (value-of exp1 env))
                           (val2 (value-of exp2 env)))
                       (let ((num1 (expval->num val1))
                             (num2 (expval->num val2)))
                         (bool-val
                          (= num1 num2)))))

         (less?-exp (exp1 exp2)
                    (let ((val1 (value-of exp1 env))
                          (val2 (value-of exp2 env)))
                      (let ((num1 (expval->num val1))
                            (num2 (expval->num val2)))
                        (bool-val
                         (< num1 num2)))))
         (greater?-exp (exp1 exp2)
                       (let ((val1 (value-of exp1 env))
                             (val2 (value-of exp2 env)))
                         (let ((num1 (expval->num val1))
                               (num2 (expval->num val2)))
                           (bool-val
                            (> num1 num2)))))

         (if-exp (exp1 exp2 exp3)
                 (if (expval->bool (value-of exp1 env))
                     (value-of exp2 env)
                     (value-of exp3 env)))
         (let-exp (vars exps body)
                  (let-exp-handler vars exps body env env))
         (let*-exp (vars exps body)
                   (let*-exp-handler vars exps body env))
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
                  (cdr
                   (expval->cons (value-of exp1 env))))
         (list-exp (args)
                   (list-val-handler (map (lambda (arg)
                                            (value-of arg env)) args)))

         (cond-exp (conds exps) (cond-exp-handler conds exps env))

         (unpack-exp (vars exp1 body) (unpack-exp-handler vars exp1 body env))

         (proc-exp (vars body) (proc-val (procedure vars body env)))
         (call-exp (rator rands)
                   (let ((proc (expval->proc (value-of rator env)))
                         (args
                          (map
                           (lambda (rand) (value-of rand env))
                           rands)))
                     (apply-procedure proc args)
                     ))

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

;; (run "
;; let makemult = proc (maker)
;;                 proc (x)
;;                    if zero?(x)
;;                    then 0
;;                    else -(((maker maker) -(x, 1)), -4)
;; in let times4 = proc (x) ((makemult makemult) x) in (times4 3)
;; ")


(run "
let* makeOdd = proc(makeOdd, makeEven)
                proc(n)
                  if zero?(n)
                  then 1
                  else ((makeEven makeEven makeOdd) -(n,1))
     makeEven = proc(makeEven, makeOdd)
                  proc(n)
                    if zero?(n)
                    then 0
                    else ((makeOdd makeOdd makeEven) -(n, 1))
in ((makeEven makeEven makeOdd) 0)
")

;; (run "
;; let x = 30
;; in let* x = -(x, 1) y = -(x,2)
;; in -(x, y)
;; ")