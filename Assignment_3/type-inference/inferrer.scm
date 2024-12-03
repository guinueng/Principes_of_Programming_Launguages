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
        (else (unification-failure ty1 ty2 exp))))))

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
