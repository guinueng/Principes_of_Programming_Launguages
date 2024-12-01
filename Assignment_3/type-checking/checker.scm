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
      (lambda-exp (var type body)
                (for-each (lambda (var type)
                            (extend-tenv ((car var) (car type) tenv))
                            (display var)
                            (display type)
                            (display "\n"))
                  (var type))
                (let ((ty (type-of body tenv)))
                  (ty)))
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

