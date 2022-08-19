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
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression (identifier) var-exp)

    (expression
     ("let" (arbno identifier "=" expression) "in" expression)
     let-exp)

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
  (zero?-exp
   (exp1 expression?))
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

(define (expval->proc val)
  (cases expval val
         (proc-val (proc) proc)
         (else (report-expval-extractor-error 'proc val))))


(define report-expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

(define (let-exp-handler vars exps body old-env new-env)
  (if (null? vars)
      (value-of body new-env)
      (let-exp-handler
       (cdr vars)
       (cdr exps)
       body
       old-env
       (extend-env (car vars) (value-of (car exps) old-env) new-env))))

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
         (zero?-exp (exp1) (bool-val (zero? (expval->num(value-of exp1 env)))))
         (if-exp (exp1 exp2 exp3)
                 (if (expval->bool (value-of exp1 env))
                     (value-of exp2 env)
                     (value-of exp3 env)))
         (let-exp (vars exps body)
                  (let-exp-handler vars exps body env env))
         (proc-exp (vars body) (proc-val (procedure vars body env)))
         (call-exp (rator rands)
                   (let ((proc (expval->proc (value-of rator env)))
                         (args
                          (map
                           (lambda (rand) (value-of rand env))
                           rands)))
                     (apply-procedure proc args)
                     ))))


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

;; (define (occurs-free? search-var exp)
;;   (cases lc-exp exp
;;          (var-exp (var) (eqv? var search-var))
;;          (lambda-exp (bound-var body)
;;                      (and
;;                       (not (eqv? search-var bound-var))
;;                            (occurs-free? search-var body)))
;;          (app-exp (rator rand)
;;                   (or
;;                    (occurs-free? search-var rator)
;;                    (occurs-free? search-var rand)))))


(define (run-free string)
  (cases program (scan&parse string)
         (a-program (exp1) (free-of exp1 `() `()))))

(define (find vars var)
  (cond ((null? vars) #f)
        ((eqv? (car vars) var) #t)
        (else (find (cdr vars) var))))

(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))

(define (free-of exp vars results)
  (cases expression exp
         (const-exp (num) results)
         (var-exp (var)
                  (cond ((null? vars) results)
                        ((find vars var) results)
                        (else (cons var results))))
         (diff-exp (exp1 exp2)
                   (append (free-of exp1 vars results)
                           (free-of exp2 vars results)))
         (zero?-exp (exp1) (free-of exp1 vars results))
         (if-exp (exp1 exp2 exp3)
                 (append (free-of exp1 vars results))
                 (append (free-of exp2 vars results))
                 (append (free-of exp3 vars results)))
         (let-exp (vars1 exps body)
                  (append (flatten (map (lambda (x) (free-of x vars `())) exps))
                          (free-of body vars results)))
         (proc-exp (vars1 body)
                   (free-of body (append vars1 vars) results))
         (call-exp (rator rands)
                   (append (free-of rator vars results)
                           (flatten (map (lambda (x) (free-of x vars results)) rands))))
         ))

;; (run-free "proc(x) -(x, y)")
;; (run-free "proc(x) -(x, 1)")
;; (run-free "let x = 4 in -(10, x)")      ;
;; (run-free "(proc(x) -(x,1)  30)")
;; (run-free "let f = proc (x) -(x,1) in (f 30)")
;; (run-free "(proc(f)(f 30)  proc(x)-(x,1))")

(run-free "
let makerec = proc (f)
        let d = proc (x)
          proc (z) ((f (x x)) z)
        in proc (n) ((f (d d)) n)
     in let maketimes4 = proc (f) proc (x)
          if zero?(x)
             then 0
          else -((f -(x,1)), z)
      in let times4 = (makerec maketimes4) in (times4 z)")
