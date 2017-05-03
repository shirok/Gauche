;;
;; This defines a compatibility module, then include Chibi's r7rs-tests to run.
;;

(use gauche.test)

;; fake (chibi test) used in r7rs-tests
(define-module chibi.test
  (use gauche.test)
  (export test-begin test-end test-values test-assert
          (rename x:test test))

  (define *nest-count* 0)

  (define (test-begin msg) (test-section msg))
  (define (test-end) #f)

  (define-syntax x:test
    (syntax-rules ()
      [(_ expected expr)
       (test (write-to-string 'expr) expected (lambda () expr)
             (lambda (a b)
               (if (and (inexact? a) (inexact? b)
                        (finite? a) (finite? b)
                        (not (zero? a)))
                 (< (abs (- a b)) (* 10e-7 (abs (+ a b))))
                 (test-check a b))))]))

  (define-syntax test-assert
    (syntax-rules ()
      [(_ str expr)
       (test str #t (lambda () (boolean expr)))]))

  (define-syntax test-values
    (syntax-rules ()
      [(_ expected expr)
       (receive r expected
         (test (write-to-string 'expr) r
               (lambda () (values->list expr))))]))
  )
(provide "chibi/test")

(define-module adaptor
  (export include)
  (define-syntax include (with-module gauche include)))
(provide "adaptor")

(test-start "r7rs-tests")

(require "r7rs")
(with-module r7rs.user
  (import (adaptor))
  (include "include/r7rs-tests.scm"))

;;
;; Some extra tests
;;

(test-section "Extra tests")

;;
;; https://github.com/shirok/Gauche/issues/221
;;
;; In 0.9.5 and before, expansion of insert-internal-define raises
;; syntax-error, since the binding of 'begin' is replaced by
;; define-library implementation.

(define-library (insert-internal-define)
  (import (scheme base))
  (export insert-internal-define)
  (begin (define-syntax insert-internal-define
           (syntax-rules ()
             ((_ x) (begin (define x 'yo!) x))))))

(define-module insert-internal-define.user
  (import insert-internal-define))

(test* "insert internal define with r7rs library" 'yo!
       (eval '(let ((a 'duh!)) (insert-internal-define a))
             (find-module 'insert-internal-define.user)))

;; cond-expand in library decl is tricky.  see the comment in r7rs.scm
(define-module cond-expand-test1.user)
(test* "cond-expand in library decl" '(a . b)
       (begin
         (eval
          '(define-library (cond-expand-test1)
             (import (scheme base))
             (export gauche:list*)
             (cond-expand
              (gauche (import (rename (only (gauche base) list*)
                                      (list* gauche:list*))))
              (else)))
          (find-module 'user))
         (eval
          '(with-module cond-expand-test1.user
             (import cond-expand-test1)
             (gauche:list* 'a 'b))
          (find-module 'user))))

;; in this test, cond-expand inserts (use srfi-42).  see we can handle it.
(define-module cond-expand-test2.user)
(test* "cond-expand in library decl 2" '(0 1 2 3 4 5 6 7 8 9)
       (begin
         (eval
          '(define-library (cond-expand-test2)
             (import (scheme base))
             (export xs)
             (cond-expand
              (srfi-42 (begin (define xs (list-ec (: x 10) x))))
              (else)))
          (find-module 'user))
         (eval
          '(with-module cond-expand-test2.user
             (import cond-expand-test2)
             xs)
          (find-module 'user))))

;; in this test, cond-expand expands into none
(test* "cond-expand in library decl 3" #t
       (begin
         (eval
          '(define-library (cond-expand-test3)
             (import (scheme base))
             (cond-expand
              (gauche)
              (else)))
          (find-module 'user))
         #t))

;;
;; Test include-library-declarations
;;

(test* "include-library-declarations 1" 5
       (begin
         (eval
          '(define-library (include-library-declarations-1)
             (include-library-declarations "include/include-library-declarations-1.scm")
             (begin (define bar (+ foo 1))))
          (find-module 'user))
         (eval
          '(begin
             (import include-library-declarations-1)
             bar)
          (make-module #f))))

(test* "include-library-declarations 2"
       (test-error <top> #/Invalid library declaration/)
       (begin
         (eval
          '(define-library (include-library-declarations-2)
             (include-library-declarations "include/include-library-declarations-2.scm")
             (begin (define bar (+ foo 1))))
          (find-module 'user))
         (eval
          '(begin
             (import include-library-declarations-2)
             bar)
          (make-module #f))))

(test-end)


