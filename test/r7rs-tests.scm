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

(test-end)


