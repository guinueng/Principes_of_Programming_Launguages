#lang racket

(require eopl)
(require "datatype.scm")
(require "environments.scm")
(require "utils.scm")

(provide value-of-program value-of)

;; value-of-program : Program * Env -> ExpVal
(define value-of-program
  (lambda (pgm env)
    (cases program pgm
      (a-program (exp)
                 (value-of exp env)))))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (debug "Evaluating exp: ~s, env: ~s~n" exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (else (error 'value-of "Unsupported expression: ~s" exp)))))