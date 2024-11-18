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

(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

(define-datatype thunk thunk?
  (a-thunk
    (exp expression?)
    (env environment?)))

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
      (call-exp (rator rand)
        (let ((proc (expval->proc (value-of rator env)))
              (arg (value-of-operand rand env)))
            (apply-procedure proc arg)))
      (else (error 'value-of "Unsupported expression: ~s" exp)))))

(define value-of-operand
  (lambda (exp env)
    (cases expression exp
      (var-exp(var) (apply-env env var))
      (else (newref (a-thunk exp env))))))

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
        (value-of body (extended-env var val saved-env))))))

(define newref
  (lambda (val)
    (let ((next-ref (length the-store)))
      (set! the-store (append the-store (list val)))
        next-ref)))
