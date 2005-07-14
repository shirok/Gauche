;;;
;;; dbd.null - A database driver that does (almost) nothing
;;;
;;;  $Id: null.scm,v 1.2 2005-07-14 09:11:19 shirok Exp $
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

(define-method dbd-make-connection ((d <null-driver>)
                                    attr-string attr-alist . options)
  (make <null-connection>
    :attr-string attr-string :attr-alist attr-alist
    :options options))

(define-method dbd-execute ((c <null-connection>) (q <dbi-query>) . params)
  (list (apply (slot-ref q '%prepared) params)))

(provide "dbd/null")

