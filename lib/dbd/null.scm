;;;
;;; dbd.null - A database driver that does (almost) nothing
;;;
;;;  $Id: null.scm,v 1.4 2005-09-07 10:51:32 shirok Exp $
;;;

(define-module dbd.null
  (use dbi)
  (export dbd-null-test-result-set!))
(select-module dbd.null)

;; The null driver is mainly for testing dbi implementation.
;; It just stores information passed via driver API, and the
;; test routine will query it later to see infos are passed
;; down to the driver correctly.

(define-class <null-driver> (<dbi-driver>)
  ())

(define-class <null-connection> (<dbi-connection>)
  ((attr-string :init-keyword :attr-string)
   (attr-alist  :init-keyword :attr-alist)
   (options     :init-keyword :options)))

(define-class <null-query> (<dbi-query>)
  ())

(define-method dbi-make-connection ((d <null-driver>)
                                    attr-string attr-alist . options)
  (make <null-connection>
    :attr-string attr-string :attr-alist attr-alist
    :options options))

(define-method dbi-prepare ((c <null-connection>) (sql <string>) . options)
  (let-keywords* options ((pass-through #f))
    (let1 prepared (if pass-through (lambda () sql) (dbi-prepare-sql c sql))
      (lambda args
        (list (apply prepared args))))))

(provide "dbd/null")

