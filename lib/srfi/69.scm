;;;
;;; SRFI-69  Basic Hash Tables
;;;

;; This is a compatibitily module.  Actual implementation is
;; in scheme/hash-table.scm.

(define-module srfi.69
  (use scheme.hash-table)
  (export (rename srfi-69:make-hash-table make-hash-table)
          hash-table? alist->hash-table
          hash-table-equivalence-function hash-table-hash-function
          hash-table-ref hash-table-ref/default
          hash-table-set! hash-table-delete!
          hash-table-exists? hash-table-update!
          hash-table-update!/default
          hash-table-size hash-table-keys hash-table-values
          hash-table-walk hash-table-fold hash-table->alist
          hash-table-copy hash-table-merge!
          hash string-hash string-ci-hash hash-by-identity))
(select-module srfi.69)

;; srfi-69's make-hash-table allows to omit equality procedure,
;; while srfi-125 does not.
(define srfi-69:make-hash-table
  (case-lambda
    [() (make-hash-table equal?)]
    [args (apply make-hash-table args)]))
