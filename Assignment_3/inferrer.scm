#lang racket

(require eopl)
(require "datatype.scm")
(require "type-environments.scm")
(require "utils.scm")
(require "parser.scm")

(provide type-of-program type-of)

;; type-of-program : Program * Env -> Type
(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp)
                 (cases answer (type-of exp (empty-tenv) '())
                   (an-answer (ty subst)
                              (debug "obtained type: ~s, subst: ~s~n" ty subst)
                              (apply-subst-to-type ty subst)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; You need to complete this function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define type-of
  (lambda (exp tenv subst)
    (debug "type-inferring exp: ~s~n" exp)
    (cases expression exp
      (const-exp (num) (an-answer (int-type) subst))
      (var-exp (var)
               (an-answer (apply-tenv tenv var) subst))
      (diff-exp (exp1 exp2)
                (cases answer (type-of exp1 tenv subst)
                  (an-answer (type1 subst1)
                             (let ((subst1 (unifier type1 (int-type) subst1 exp1)))
                               (cases answer (type-of exp2 tenv subst1)
                                 (an-answer (type2 subst2)
                                            (let ((subst2
                                                   (unifier type2 (int-type) subst2 exp2)))
                                              (an-answer (int-type) subst2))))))))
      (add-exp (exp1 exp2)
                (cases answer (type-of exp1 tenv subst)
                  (an-answer (type1 subst1)
                              (let ((subst1 (unifier type1 (int-type) subst1 exp1)))
                                (cases answer (type-of exp2 tenv subst1)
                                  (an-answer (type2 subst2)
                                            (let ((subst2 (unifier type2 (int-type) subst2 exp2)))
                                              (an-answer (int-type) subst2))))))))
      (zero?-exp (exp)
                (cases answer (type-of exp tenv subst)
                  (an-answer (ty1 subst1)
                              (let ((subst1 (unifier ty1 (int-type) subst1 exp)))
                                (an-answer (bool-type) subst1)))))
      (less-than-exp (exp1 exp2)
                (cases answer (type-of exp1 tenv subst)
                  (an-answer (ty1 subst1)
                              (let ((subst1 (unifier ty1 (int-type) subst1 exp1)))
                                (cases answer (type-of exp2 tenv subst1)
                                  (an-answer (ty2 subst2)
                                            (let ((subst2 (unifier ty2 (int-type) subst2 exp2)))
                                              (an-answer (bool-type) subst2))))))))
      (not-exp (exp)
                (cases answer (type-of exp tenv subst)
                  (an-answer (ty1 subst1)
                              (let ((subst1 (unifier ty1 (bool-type) subst1 exp)))
                                (an-answer (bool-type) subst1)))))
      (if-exp (exp1 exp2 exp3)
                (cases answer (type-of exp1 tenv subst)
                  (an-answer (ty1 subst1)
                              (let ((subst1 (unifier ty1 (bool-type) subst1 exp1)))
                                (cases answer (type-of exp2 tenv subst1)
                                  (an-answer (ty2 subst2)
                                    (cases answer (type-of exp3 tenv subst2)
                                      (an-answer (ty3 subst3)
                                                (let ((subst3 (unifier ty2 ty3 subst3 exp2)))
                                                  (an-answer ty2 subst3))))))))))
      (lambda-exp (bound-vars var-tys body)
                (cases answer (type-of body (extend-tenv-list bound-vars var-tys tenv) subst)
                  (an-answer (body-ty subst1)
                              (an-answer (proc-type var-tys body-ty) subst1))))
      (app-exp (rator rand)
                (let ((rst-ty (fresh-tvar-type)))
                  (cases answer (type-of rator tenv subst)
                    (an-answer (rator-ty subst1)
                      (let ((subst2 (app-exp-rec rand rator-ty rst-ty tenv subst1 exp (list))))
                        (an-answer rst-ty subst2))))))
      (let-exp (vars exps body)
        (let ((extended-tenv
                (extend-tenv-list vars (map (lambda (exp) (cases answer (type-of exp tenv subst) (an-answer (exp-ty subst) exp-ty))) exps) tenv)))
          (type-of body extended-tenv subst)))
      (letrec-exp (rst-tys proc-names lambda-exps body)
        (let ((extended-tenv (extend-tenv-list proc-names (map (lambda (p-arg rst-ty) (proc-type p-arg rst-ty)) (map (lambda (l-exp) (lambda-exp->var-types l-exp)) lambda-exps) rst-tys) tenv)))
          (let ((subst1 (letrec-subst-extend lambda-exps rst-tys extended-tenv subst)))
            (type-of body extended-tenv subst1))))

      (begin-exp (exps)
        (let ((rator (car exps))
              (rand (cdr exps)))
          (if (null? rand)
            (type-of rator tenv subst)
            (type-of (begin-exp rand) tenv subst))))
      (else (error (format "type-of: not implemented: ~s" exp))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; You need to complete this function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define unifier
  (lambda (ty1 ty2 subst exp)
    (debug "unifying ~s and ~s~n" ty1 ty2)
    (let ((ty1 (apply-subst-to-type ty1 subst))
          (ty2 (apply-subst-to-type ty2 subst)))
      (cond
        ((equal? ty1 ty2)
         subst)
        ((tvar-type? ty1)
          (if (no-occurrence? ty1 ty2)
            (extend-subst subst ty1 ty2)
            (unification-failure ty1 ty2 exp)))
        ((tvar-type? ty2)
          (if (no-occurrence? ty2 ty1)
            (extend-subst subst ty2 ty1)
            (unification-failure ty2 ty1 exp)))
        ((and (proc-type? ty1) (proc-type? ty2)
            (let ((subst1 (unwrap (map (lambda (ty1 ty2) (unifier ty1 ty2 subst exp)) (proc-type->arg-types ty1) (proc-type->arg-types ty2)))))
              (let ((subst2 (unifier (proc-type->result-type ty1) (proc-type->result-type ty2) subst1 exp)))
                subst2))))
        (else (unification-failure ty1 ty2 exp))))))

(define app-exp-rec
  (lambda (rands rator-ty rst-ty tenv subst exp rand-tys)
    (let ((rand-answer (type-of (car rands) tenv subst)))
      (debug "unifying-rec ~s and ~s~n" rand-answer rator-ty)
      (cases answer rand-answer
        (an-answer (rand-ty subst1)
          (let ((rand-tys (append rand-tys (list rand-ty))))
            (if (null? (cdr rands))
              (unifier (proc-type rand-tys rst-ty) rator-ty subst1 exp)
              (app-exp-rec (cdr rands) rator-ty rst-ty tenv subst1 exp rand-tys))))))))

(define letrec-subst-extend
  (lambda (lambda-exps rst-tys extended-tenv subst)
    (if (null? (cdr lambda-exps))
      (cases answer (type-of (car lambda-exps) extended-tenv subst)
                        (an-answer (exp-ty subst1)
                          (unifier (proc-type->result-type exp-ty) (car rst-tys) subst1 (car lambda-exps))))
      (letrec-subst-extend (cdr lambda-exps) (cdr rst-tys) extended-tenv
            (unifier (cases answer (type-of (car lambda-exps) extended-tenv subst)
                        (an-answer (exp-ty subst1)
                          (proc-type->result-type exp-ty))) (car rst-tys) subst (car lambda-exps))))))

(define unwrap
  (lambda (lst)
    (if (null? (cdr lst))
      (car lst)
      (unwrap (cdr lst)))))

(define apply-subst-to-type
  (lambda (ty subst)
    (cases type ty
      (int-type () (int-type))
      (bool-type () (bool-type))
      (proc-type (arg-types result-type)
                 (proc-type
                  (map (lambda (arg-type)
                         (apply-subst-to-type arg-type subst))
                       arg-types)
                  (apply-subst-to-type result-type subst)))
      (tvar-type (id)
                 (let ((ty-val-pair (assoc ty subst)))
                   (if ty-val-pair
                       (cdr ty-val-pair)
                       ty))))))

;; apply-one-subst: type * tvar * type -> type
;; (apply-one-subst ty0 var ty1) returns the type obtained by
;; substituting ty1 for every occurrence of tvar in ty0.
(define apply-one-subst
  (lambda (ty0 tvar ty1)
    (cases type ty0
      (int-type () (int-type))
      (bool-type () (bool-type))
      (proc-type (arg-type result-type)
                 (proc-type
                  (map (lambda (arg-type)
                         (apply-one-subst arg-type tvar ty1))
                       arg-type)
                  (apply-one-subst result-type tvar ty1)))
      (tvar-type (id)
                 (if (equal? ty0 tvar) ty1 ty0)))))

(define extend-subst
  (lambda (subst tvar ty)
    (cons
     (cons tvar ty)
     (map
      (lambda (p)
        (let ((oldlhs (car p))
              (oldrhs (cdr p)))
          (cons
           oldlhs
           (apply-one-subst oldrhs tvar ty))))
      subst))))

(define no-occurrence?
  (lambda (tvar ty)
    (cases type ty
      (int-type () #t)
      (bool-type () #t)
      (proc-type (arg-types result-type)
                 (and
                  (foldl (lambda (arg-type acc)
                           (and acc (no-occurrence? tvar arg-type)))
                         #t
                         arg-types)
                  (no-occurrence? tvar result-type)))
      (tvar-type (id) (not (equal? tvar ty))))))

(define unification-failure
  (lambda (ty1 ty2 exp)
    (debug "unification failure in ~s: ~s (actual) != ~s (expected)"
           exp
           (pretty-print ty1)
           (pretty-print ty2))
    (error (format "unification failure: ~s (actual) != ~s (expected)"
                   (pretty-print ty1)
                   (pretty-print ty2)))))

; 20211216 GwanUk Lee
;
; Type Inference.
;
; In const-exp to lambda-exp, it can be re-used as formal form in checker.scm.
; But main difference is it utilize subst, which tenv collects all types and subst saves combination of target types.
; In newref-exp, setref-exp, deref-exp, and assign-exp it is not used in evaluation, thus I did not implemented it.
; In app-exp, let-exp, letref-exp, first I implemented by utilizing recursion, but we need to check whole type of result types and lamba-exps result types.
; For utilizing recursion, it is impossible to compare types in sequential way, thus I changed implementation by utilizing map function and recursion both.
; Additionally implemented functions are app-exp-rec, letrec-subst-extend, and unwrap.
; Function unwrap unwraps list and returns final value of list.
; Function app-exp-rec recurse to get list of result types operand types and finally it calls unifier to compare procedure type which contains rand type and result types with rator type.
; Function letrec-subst-extend is designed to extend subst in sequential way, and returns subst finally.
;
; Reference:
; 1. Lecture Note Given by Professor.
; 2. Textbook.
; 3. https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Mapping-of-Lists.html