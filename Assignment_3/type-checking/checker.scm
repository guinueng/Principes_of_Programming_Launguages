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
      (app-exp (rator rands)
                (let ((rator-ty (type-of rator tenv))
                      (rand-ty (map (lambda (rand) (type-of rand tenv)) rands)))
                      (cases type rator-ty
                        (proc-type (arg-ty rst-ty)
                          (check-equal-type-list! rand-ty arg-ty rands)
                          rst-ty)
                        (int-type ()
                          (check-equal-type! rand-ty rator-ty rands)
                          rator-ty)
                        (bool-type ()
                          (check-equal-type! rand-ty rator-ty rands)
                          rator-ty))))
                ; Original implementation of app-exp
                ; (define rand-ty-list
                ;  (lambda (rand-list rand-list-source)
                ;    (when (not (null? rand-list))
                ;      (let ((rst-list (list (type-of (car rand-list) tenv) (car rand-list-source))))```
                ;        (if (null? (cdr rand-list))
                ;          rst-list
                ;          (rand-ty-list (cdr rand-list) rst-list))))))
                ; (let ((rator-ty (type-of rator tenv))
                ;      (rand-init-ty (list (type-of (car rand) tenv))))
                ;   (let ((rand-ty
                ;          (if (not (null? (cdr rand)))
                ;            (rand-ty-list (cdr rand) rand-init-ty)
                ;            rand-init-ty)))
                ;     (cases type rator-ty
                ;      (proc-type (arg-ty rst-ty)
                ;        (check-equal-type-list! rand-ty arg-ty rand)
                ;        rst-ty)
                ;       (int-type ()
                ;        (check-equal-type! (int-type) rand-ty rand)
                ;         (int-type))
                ;       (bool-type ()
                ;        (check-equal-type! (bool-type) rand-ty rand)
                ;         (bool-type)))))
      (let-exp (vars exps body)
                (let ((extended-tenv (extend-tenv-list vars (map (lambda (var) (type-of var tenv)) exps) tenv)))
                  (type-of body extended-tenv)))
                ; Original implementation of let-exp
                ; (let ((var (car vars))
                ;       (ty (type-of (car exps) tenv)))
                ;   (if (null? (cdr vars))
                ;     (type-of body (extend-tenv var ty tenv))
                ;     (type-of (let-exp (cdr vars) (cdr exps) body) (extend-tenv var ty tenv))))
      (letrec-exp (rst-tys proc-names lambda-exps body)
                (let ((extended-types (extend-tenv-list proc-names (map (lambda (p-arg p-rst) (proc-type p-arg p-rst)) (map (lambda (l-exp) (lambda-exp->var-types l-exp)) lambda-exps) rst-tys) tenv)))
                  (check-equal-type-list! (map (lambda (lambda-exp) (proc-type->result-type (type-of lambda-exp extended-types))) lambda-exps) rst-tys lambda-exps)
                  (type-of body extended-types)))
                ; Original implementation of letrec-exp
                ; (if (null? (cdr rst-tys))
                ;   (let ((body-ty
                ;         (type-of body (extend-tenv (car (lambda-exp->bound-vars (car lambda-exps))) (car (lambda-exp->var-types (car lambda-exps)))
                ;           (extend-tenv (car proc-names) (proc-type (lambda-exp->var-types (car lambda-exps)) (car rst-tys)) tenv)))))
                ;       (cases type body-ty
                ;         (proc-type (arg-ty rst-ty)
                ;           (check-equal-type! (car rst-tys) (car arg-ty) body)
                ;           body-ty)
                ;         (int-type ()
                ;           (check-equal-type! (int-type) (car rst-tys) body)
                ;           body-ty)
                ;         (bool-type ()
                ;           (check-equal-type! (bool-type) (car rst-tys) body)
                ;           body-ty)))
                ;       body-ty)
                ;   (type-of (letrec-exp (cdr rst-tys) (cdr proc-names) (cdr lambda-exps) body)
                ;             (extend-tenv (car proc-names) (proc-type (lambda-exp->var-types (car lambda-exps)) (car rst-tys)) tenv))))
      (begin-exp (exps)
        (let ((rator (car exps))
              (rand (cdr exps)))
          (let ((ty (type-of rator tenv)))
            (if (null? rand)
              ty
              (type-of (begin-exp rand) tenv)))))
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

(define proc-type->result-type
  (lambda (t)
    (cases type t
      (proc-type (arg-types result-type) result-type)
      (else (error (format "proc-type->result-type: not a proc-type: ~s" t))))))

; 20211216 GwanUk Lee
;
; Type Checker.
;
; In const-exp to lambda-exp, it can be re-used as formal form in assignment_2.
; But main difference is it gets types and check it has equal types then returns result types.
; In newref-exp, setref-exp, desref-exp,, and assign-exp it is not used in evaluation, thus I did not implemented it.
; In app-exp, let-exp, letref-exp, first I implemented by utilizing recursion, but we need to check whole type of result types and lamba-exps result types.
; For utilizing recursion, it is impossible to compare types in sequential way, thus I changed implementation by utilizing map function.
; Also, I brought proc-type->result-type function on type-inference's datatype.scm to utilize extract lambda-exp's result type.
;
; Reference:
; 1. Lecture Note Given by Professor.
; 2. Textbook.
; 3. Skeleton Code on checker.scm
; 3. https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Mapping-of-Lists.html