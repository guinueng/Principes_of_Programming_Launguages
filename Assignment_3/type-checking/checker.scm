#lang racket

(require eopl)
(require "datatype.scm")
(require "type-environments.scm")
(require "utils.scm")

(provide type-of-program type-of)

;; type-of-program : Program * Env -> Type
(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp)
                 (type-of exp (empty-tenv))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; You need to complete this function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define type-of
  (lambda (exp tenv)
    (debug "type-checking exp: ~s~n" exp)
    (cases expression exp
      (const-exp (num) (int-type))
      (var-exp (var) (apply-tenv tenv var))
      (diff-exp (exp1 exp2)
                (let ((ty1 (type-of exp1 tenv))
                      (ty2 (type-of exp2 tenv)))
                  (check-equal-type! ty1 (int-type) exp1)
                  (check-equal-type! ty2 (int-type) exp2)
                  (int-type)))
      (add-exp (exp1 exp2)
                (let ((ty1 (type-of exp1 tenv))
                      (ty2 (type-of exp2 tenv)))
                  (check-equal-type! ty1 (int-type) exp1)
                  (check-equal-type! ty2 (int-type) exp2)
                  (int-type)))
      (zero?-exp (exp)
                  (let ((ty (type-of exp tenv)))
                    (check-equal-type! ty (int-type) exp)
                    (bool-type)))
      (less-than-exp (exp1 exp2)
                      (let ((ty1 (type-of exp1 tenv))
                            (ty2 (type-of exp2 tenv)))
                        (check-equal-type! ty1 (int-type) exp1)
                        (check-equal-type! ty2 (int-type) exp2)
                        (bool-type)))
      (not-exp (exp)
                (let ((ty (type-of exp tenv)))
                  (check-equal-type! ty (bool-type) exp)
                  (bool-type)))
      (if-exp (exp1 exp2 exp3)
                (let ((ty1 (type-of exp1 tenv))
                      (ty2 (type-of exp2 tenv))
                      (ty3 (type-of exp3 tenv)))
                      (check-equal-type! ty1 (bool-type) exp1)
                      (check-equal-type! ty2 ty3 exp2)
                      ty2))
      (lambda-exp (vars types body)
                (proc-type types (type-of body (extend-tenv-list vars types tenv))))
      (app-exp (rator rand)
                (define rand-ty-list
                  (lambda (rand-list rand-list-source)
                    (when (not (null? rand-list))
                      (let ((rst-list (list (type-of (car rand-list) tenv) (car rand-list-source))))
                        (if (null? (cdr rand-list))
                          rst-list
                          (rand-ty-list (cdr rand-list) rst-list))))))
                (let ((rator-ty (type-of rator tenv))
                      (rand-init-ty (list (type-of (car rand) tenv))))
                  (let ((rand-ty
                          (if (not (null? (cdr rand)))
                            (rand-ty-list (cdr rand) rand-init-ty)
                            rand-init-ty)))
                    (cases type rator-ty
                      (proc-type (arg-ty rst-ty)
                        (check-equal-type-list! arg-ty rand-ty rand)
                        rst-ty)
                      (int-type ()
                        (check-equal-type! (int-type) rand-ty rand)
                        (int-type))
                      (bool-type ()
                        (check-equal-type! (bool-type) rand-ty rand)
                        (bool-type))))))
      (let-exp (vars exps body)
                (let ((var (car vars))
                      (ty (type-of (car exps) tenv)))
                  (if (null? (cdr vars))
                    (type-of body (extend-tenv var ty tenv))
                    (type-of (let-exp (cdr vars) (cdr exps) body) (extend-tenv var ty tenv)))))
      (letrec-exp (rst-ty proc-name lambda-exp body)
                (let ((tenv-letrec-body
                  (extend-tenv proc-name (proc-type (lambda-exp->var-types lambda-exp) rst-ty) tenv)))
                  (let ((body-ty (type-of body (extend-tenv (lambda-exp->bound-vars lambda-exp) tenv-letrec-body))))
                    (check-equal-type! body-ty rst-ty body)
                    (type-of body tenv-letrec-body))))
      (else (error (format "type-of: not implemented: ~s" exp))))))

(define check-equal-type!
  (lambda (ty1 ty2 exp)
    (when (not (equal? ty1 ty2))
      (debug "type-error in ~s: ~s (actual) != ~s (expected)~n"
             exp (pretty-print ty1) (pretty-print ty2))
      (error (format "type-error: ~s (actual) != ~s (expected)"
                     (pretty-print ty1) (pretty-print ty2))))))

(define check-equal-type-list!
  (lambda (tys1 tys2 exps)
    (when (not (null? tys1))
      (begin
        (check-equal-type! (car tys1) (car tys2) (car exps))
        (check-equal-type-list! (cdr tys1) (cdr tys2) (cdr exps))))))

;; test