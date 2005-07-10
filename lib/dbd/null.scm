;;;
;;; dbd.null - A database driver that does (almost) nothing
;;;
;;;  Public domain.
;;;  DBD driver writer may use this file to start with.
;;;
;;;  $Id: null.scm,v 1.1 2005-07-10 20:39:18 shirok Exp $
;;;

(define-module dbd.null
  (use dbi)
  (export dbd-null-test-result-set!))
(select-module dbd.null)

;;;==============================================================
;;; DBI objects
;;;

;; The driver should define subclass of <dbi-driver>, <dbi-connection>,
;; <dbi-query> and <dbi-result-set>.
;; The driver class name should be #`"<,|driver-name|-driver>".
;; These class names don't need to be exported.

(define-class <null-driver> (<dbi-driver>)
  ())

(define-class <null-connection> (<dbi-connection>)
  ())

(define-class <null-query> (<dbi-query>)
  ((connection :init-keyword :connection)))

;;;==============================================================
;;; DBI method implementations
;;;

(define-method dbi-make-connection ((d <null-driver>) . options)
  (make <null-connection>))

(define-method dbi-prepare-query



(provide "dbd/null")

