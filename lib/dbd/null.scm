;;;
;;; dbd.null - A database driver that does (almost) nothing
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
   (options     :init-keyword :options)
   (open?       :init-value #t)))

(define-method dbi-make-connection ((d <null-driver>)
                                    attr-string attr-alist . options)
  (make <null-connection>
    :attr-string attr-string :attr-alist attr-alist
    :options options))

(define-method dbi-execute-using-connection ((c <null-connection>) q p)
  (list (apply (ref q 'prepared) p)))

(define-method dbi-open? ((c <null-connection>))
  (ref c 'open?))

(define-method dbi-close ((c <null-connection>))
  (set! (ref c 'open?) #f))


