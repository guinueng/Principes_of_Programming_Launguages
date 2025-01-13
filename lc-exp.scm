#lang racket

;; var-exp : Var -> Lc-exp
(define var-exp
  (lambda (var)
    `(var-exp ,var)))

;; lambda-exp : Var * Lc-exp -> Lc-exp
(define lambda-exp
  (lambda (var lc-exp)
    `(lambda-exp ,var ,lc-exp)))

;; app-exp : Lc-exp * Lc-exp -> Lc-exp
(define app-exp
  (lambda (lc-exp1 lc-exp2)
    `(app-exp ,lc-exp1 ,lc-exp2)))

;; var-exp? : Lc-exp -> Bool
(define var-exp?
  (lambda (x)
    (and (pair? x) (eqv? (car x) 'var-exp))))

;; lambda-exp? : Lc-exp -> Bool
(define lambda-exp?
  (lambda (x)
    (and (pair? x) (eqv? (car x) 'lambda-exp))))

;; app-exp? : Lc-exp -> Bool
(define app-exp?
  (lambda (x)
    (and (pair? x) (eqv? (car x) 'app-exp))))

;; var-exp->var : Lc-exp -> Var
(define var-exp->var
  (lambda (x)
    (cadr x)))

;; lambda-exp->bound-var : Lc-exp -> Var
(define lambda-exp->bound-var
  (lambda (x)
    (cadr x)))

;; lambda-exp->body : Lc-exp -> Lc-exp
(define lambda-exp->body
  (lambda (x)
    (caddr x)))

;; app-exp->rator : Lc-exp -> Lc-exp
(define app-exp->rator
  (lambda (x)
    (cadr x)))

;; app-exp->rand : Lc-exp -> Lc-exp
(define app-exp->rand
  (lambda (x)
    (caddr x)))

;; occurs-free? : Sym * Lcexp -> Bool
(define occurs-free?
  (lambda (search-var exp)
    (cond
      ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
      ((lambda-exp? exp)
       (and
        (not (eqv? search-var (lambda-exp->bound-var exp)))
        (occurs-free? search-var (lambda-exp->body exp))))
      (else
       (or
        (occurs-free? search-var (app-exp->rator exp))
        (occurs-free? search-var (app-exp->rand exp)))))))

;; parse-expression : Schemeval -> Lcexp
(define parse-expression
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      ((pair? datum)
       (if (eqv? (car datum) 'lambda)
           (lambda-exp
            (car (cadr datum))
            (parse-expression (caddr datum)))
           (app-exp
            (parse-expression (car datum))
            (parse-expression (cadr datum)))))
      (else (report-invalid-concrete-syntax datum)))))

(define report-invalid-concrete-syntax
  (lambda (datum)
    (printf "invalid concrete syntax ~s" datum)))

;; a few small unit tests
(printf "~a~n" (var-exp 'x))
(printf "~a~n" (lambda-exp 'x 'x))
(printf "~a~n" (var-exp? (var-exp 'x)))
(printf "~a~n" (lambda-exp? (var-exp 'x)))
(printf "~a~n" (lambda-exp? (lambda-exp 'x 'x)))

(printf "~a~n" (parse-expression '((lambda (x) x) (x y))))

(parse-expression '((lambda (x) x) (x y)))

(equal?
 (occurs-free? 'a (lambda-exp 'a (app-exp (var-exp 'b) (var-exp 'a))))
 #f)

(equal?
 (occurs-free? 'b (lambda-exp 'a (app-exp (var-exp 'b) (var-exp 'a))))
 #t)

(equal?
 (occurs-free? 'x (parse-expression '((lambda (x) x) (x y))))
 #t)