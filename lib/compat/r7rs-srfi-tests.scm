;;;
;;; compat.r7rs-srfi-tests
;;;
;;;  This is a simple adapter to run test written in r7rs define-library
;;;  form in Gauche without loading r7rs.
;;;
;;;  What it does is basically redefining define-library locally to extract
;;;  expressions in 'begin', with ignoring other library declarations.
;;;  Necessary setup (e.g. imports) must be done in the caller side.
;;;
;;;  The reason we need this is that we run srfi tests _before_ testing full
;;;  r7rs functionality, so we don't want to load r7rs in order to test given
;;;  srfi tests.  So far, most of r7rs srfi tests doesn't use elaborated r7rs
;;;  features, and simple adapter suffices our purpose.  (If the test does
;;;  require full r7rs features, it must be tested after r7rs tests.)
;;;

(define-module compat.r7rs-srfi-tests
  (export define-library))
(select-module compat.r7rs-srfi-tests)

(define-syntax define-library
  (er-macro-transformer
   (^[f r c]
     (find (^[form] (and (pair? form) (c (r'begin) (car form))))
           (cddr f)))))
