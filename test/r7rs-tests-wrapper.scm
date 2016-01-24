;; wrapper of r7rs  -*- coding: utf-8 -*-

;; fake (chibi test) used in r7rs-tests
(define-module chibi.test
  (use gauche.test)
  (export test-begin test-end test-values (rename x:test test))

  (define *nest-count* 0)

  (define (test-begin msg)
    (if (zero? *nest-count*)
      (test-start msg)
      (test-section msg))
    (inc! *nest-count*))

  (define (test-end)
    (dec! *nest-count*)
    (when (zero? *nest-count*)
      (with-module gauche.test (test-end))))

  (define-syntax x:test
    (syntax-rules ()
      [(_ expected expr)
       (test (write-to-string 'expr) expected (lambda () expr)
             (lambda (a b)
               (if (and (inexact? a) (inexact? b)
                        (finite? a) (finite? b)
                        (not (zero? a)))
                 (< (abs (- a b)) (* 10e-14 (abs (+ a b))))
                 (test-check a b))))]))

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

(require "r7rs")
(select-module r7rs.user)
(import (adaptor))
(include "include/r7rs-tests.scm")
