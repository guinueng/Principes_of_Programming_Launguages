#lang racket

(require eopl)
(require "datatype.scm")
(require "environments.scm")
(require "utils.scm")
(require "store.scm")

(provide value-of-program value-of)

(define reference-mode 'unspecified)

;; value-of-program : Program * Env -> ExpVal
(define value-of-program
  (lambda (pgm env)
    (initialize-store!)
    (cases program pgm
      (a-program (exp)
                 (value-of exp env)))))

(define value-of-operand
  (lambda (exp env)
    (cases expression exp
      (var-exp (var) (apply-env env var))
      (else (newref (a-thunk exp env))))))

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
        (value-of body (extend-env var val saved-env))))))

(define value-of-thunk
  (lambda (th)
    (cases thunk th
      (a-thunk (exp1 saved-env)
        (value-of exp1 saved-env)))))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (debug "Evaluating exp: ~s, env: ~s~n" exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (add-exp (exp1 exp2)
               (let ((result (+ (expval->num (value-of exp1 env))
                                (expval->num (value-of exp2 env)))))
                 (num-val result)))
      (diff-exp (exp1 exp2)
                (let ((result (- (expval->num (value-of exp1 env))
                                 (expval->num (value-of exp2 env)))))
                  (num-val result)))
      (zero?-exp (exp)
                 (let ((result (zero? (expval->num (value-of exp env)))))
                   (bool-val result)))
      (less-than-exp (exp1 exp2)
                     (let ((result (< (expval->num (value-of exp1 env))
                                      (expval->num (value-of exp2 env)))))
                       (bool-val result)))
      (not-exp (exp)
               (let ((result (not (expval->bool (value-of exp env)))))
                 (bool-val result)))
      (if-exp (test then else)
              (if (expval->bool (value-of test env))
                  (value-of then env)
                  (value-of else env)))
      (lambda-exp (bound-var body)
                  (proc-val (procedure bound-var body env)))
      (var-exp (var)
        (let ((ref1 (apply-env env var)))
          (let ((w (deref ref1)))
            (if (expval? w)
              w
              (let ((val (value-of-thunk w)))
                (begin
                  (setref! ref1 val)
                  val))))))
      (app-exp (rator rand)
        (let ((proc (expval->proc (value-of rator env)))
            (val (value-of-operand rand env)))
            (apply-procedure proc val)))
      (let-exp (var exp body)
        (let ((val (value-of-operand exp env)))
          (value-of body (extend-env var val env))))
      (letrec-exp (proc-name lambda_exp letrec-body)
        (value-of letrec-body (extend-env-rec proc-name lambda_exp env)))
      (begin-exp (exp)
        (let ((rator (car exp))
              (rand (cdr exp)))
          (let ((val (value-of rator env)))
            (if (null? rand)
              (expval->num val)
              (value-of (begin-exp rand) env)))))
      (newref-exp (exp)
        (let ((val (value-of exp env)))
          (ref-val (newref val))))
      (deref-exp (exp)
        (let ((val (value-of exp env)))
          (let ((ref (expval->ref val)))
            (deref ref))))
      (setref-exp (exp1 exp2)
        (let ((ref (expval->ref (value-of exp1 env))))
          (let ((val (value-of exp2 env)))
            (begin
              (setref! ref val)
              val))))
      (assign-exp (var exp)
        (let ((val (value-of exp env)))
          (setref!
            (apply-env env var)
            val)
          val))
      (else (error 'value-of "Unsupported expression: ~s" exp)))))

; 20211216 GwanUk Lee
;
; Call by name intepreter.
;
; In implicit form and call by name, we can reuse function in call by reference.
; Modified function would be var-exp, app-exp, value-of-operand, apply-procedure.
; Newly added function would be value-of-thunk.
; Modified function is modified as it can use thunk.
; In let function, we get exp value as value-of-operand which will evaluate later it needs.
; value-of-operand returns reference, thus we can directly input reference by utilizing extend-env.
; value-of-thunk is added to due added datatype thunk.
;
; Reference:
; 1. Lecture Note Given by Professor.
; 2. Textbook.
; 3. https://docs.racket-lang.org/reference/pairs.html
;