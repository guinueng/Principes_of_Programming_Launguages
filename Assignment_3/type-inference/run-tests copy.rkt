#lang racket

(require "test-infra.scm")
(require "parser.scm")
(require "datatype.scm")
(require "inferrer.scm")
(require "utils.scm")

;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;

(define test-list
  '(
    ;; Format:
    ;; (test-name explicit-or-implicit-ref program environment expected-output)
    (rec-proc-1 "(letrec ((int f (lambda ((x int)) (if (zero? x) 0 (- 1 (f ((+ x 1)))))))) (f (0)))" int)
    (rec-proc-2 "(letrec ((int f (lambda ((x ?)) (if (zero? x) 0 (- 1 (f ((+ x 1)))))))) (f (0)))" int)
    (rec-proc-3 "(letrec ((? f (lambda ((x int)) (+ x 1)))) (f (0)))" int)
    (rec-proc-4 "(letrec ((? f (lambda ((x ?)) (+ x 1)))) (f (0)))" int)
    (rec-proc-5 "(letrec ((int f (lambda ((x int)) (if (zero? x) 0 (+ 1 (f ((- x 1)))))))) f)" (int -> int))
    (rec-proc-6 "(letrec ((? f (lambda ((x int)) (if (zero? x) 0 (+ 1 (f ((- x 1)))))))) f)" (int -> int))
    (rec-proc-7 "(letrec ((int f (lambda ((x ?)) (if (zero? x) 0 (+ 1 (f ((- x 1)))))))) f)" (int -> int))
    (rec-proc-8 "(letrec ((bool f (lambda ((x int)) (zero? x)))) f)" (int -> bool))
    (rec-proc-9 "(letrec ((bool f (lambda ((x ?)) (zero? x)))) f)" (int -> bool))
    (rec-proc-10 "(letrec ((? f (lambda ((x ?)) (zero? x)))) f)" (int -> bool))
    (mut-rec-proc-1 "(letrec ((int even (lambda ((x int)) (if (zero? x) 1 (odd ((- x 1)))))) (int odd (lambda ((x int)) (if (zero? x) 0 (even ((- x 1))))))) (odd (1)))" int)
    (mut-rec-proc-2 "(letrec ((int even (lambda ((x ?)) (if (zero? x) 1 (odd ((- x 1)))))) (int odd (lambda ((x ?)) (if (zero? x) 0 (even ((- x 1))))))) (odd (1)))" int)
    (mut-rec-proc-3 "(letrec ((? even (lambda ((x ?)) (if (zero? x) 1 (odd ((- x 1)))))) (int odd (lambda ((x ?)) (if (zero? x) 0 (even ((- x 1))))))) (odd (1)))" int)
    (mut-rec-proc-4 "(letrec ((? even (lambda ((x ?)) (if (zero? x) 1 (odd ((- x 1)))))) (? odd (lambda ((x ?)) (if (zero? x) 0 (even ((- x 1))))))) (odd (1)))" int)
    (mut-rec-proc-5 "(letrec ((int even (lambda ((x int)) (if (zero? x) 1 (odd ((- x 1)))))) (int odd (lambda ((x int)) (if (zero? x) 0 (even ((- x 1))))))) (even (2)))" int)
    (mut-rec-proc-6 "(letrec ((? even (lambda ((x ?)) (if (zero? x) 1 (odd ((- x 1)))))) (? odd (lambda ((x ?)) (if (zero? x) 0 (even ((- x 1))))))) (even (2)))" int)
    (mut-rec-proc-3 "(letrec ((int even (lambda ((x int)) (if (zero? x) 1 (odd ((- x 1)))))) (int odd (lambda ((x int)) (if (zero? x) 0 (even ((- x 1))))))) even)" (int -> int))
    (mut-rec-proc-4 "(letrec ((int even (lambda ((x ?)) (if (zero? x) 1 (odd ((- x 1)))))) (int odd (lambda ((x ?)) (if (zero? x) 0 (even ((- x 1))))))) even)" (int -> int))
    (mut-rec-proc-5 "(letrec ((? even (lambda ((x ?)) (if (zero? x) 1 (odd ((- x 1)))))) (? odd (lambda ((x ?)) (if (zero? x) 0 (even ((- x 1))))))) even)" (int -> int))
    (mut-rec-proc-6 "(letrec ((? even (lambda ((x ?)) (if (zero? x) 1 (odd ((- x 1)))))) (? odd (lambda ((x ?)) (if (zero? x) 0 (even ((- x 1))))))) odd)" (int -> int))
    (if-exp-type-error-1 "(if 5 1 2)" "unification failure: int (actual) != bool (expected)")
    (if-exp-type-error-2 "(if (zero? 1) (zero? 10) 2)" "unification failure: bool (actual) != int (expected)")
    (if-exp-type-error-3 "(if (zero? 1) 100 (zero? 10))" "unification failure: int (actual) != bool (expected)")
    (app-type-error-1 "((lambda ((x bool)) x) (3))" "unification failure: int (actual) != bool (expected)")
    (app-type-error-2 "((lambda ((x int) (y bool)) (if y (+ x 1) (- x 1))) (3 5))" "unification failure: int (actual) != bool (expected)")
    (letrec-type-error-1 "(letrec ((int f (lambda ((x int)) (if (zero? x) 0 (+ 1 (f ((- x 1)))))))) (f ((zero? 11))))" "unification failure: bool (actual) != int (expected)")
    (letrec-type-error-2 "(letrec ((bool f (lambda ((x int)) (if (zero? x) 0 (+ 1 (- x 1)))))) f)" "unification failure: int (actual) != bool (expected)")
    (letrec-type-error-3 "(letrec ((bool f (lambda ((x int)) (if (zero? x) 0 (+ 1 (f ((- x 1)))))))) f)" "unification failure: bool (actual) != int (expected)")
    ))

;; run : String -> ExpVal
(define run
  (lambda (string)
    (init-var-type-id!)
    (let ((ast (parse string)))
      (begin
        (debug "program: ~a~%" string)
        (debug "ast: ~a~%" ast)
        (let ((result (type-of-program ast)))
          (debug "result: ~a~%" result)
          result)))))

(define run-all
  (lambda ()
    (run-tests! run equal-answer? test-list)))

(run-all)