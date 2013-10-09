;;
;; Generate lib/srfi/N.scm
;; Run this with $top_builddir as an argument.
;;
;; Note: Any file with .scm suffix in this directory will be deleted
;; by 'make clean'.

(use util.match)
(use file.util)

(define *supported-srfis*
  '(0 1 2 4 5 6 7 8 9 10 11 13 14 16 17 18 19 22 23 25 26 27 28 29
    30 31 34 35 36 37 38 39 40 42 43 45 55 60 61 62 87 95 98 99
    106))

(define (generate top_builddir)
  (make-directory* (build-path top_builddir "lib/srfi"))
  (dolist [srfi *supported-srfis*]
    (with-output-to-file (build-path top_builddir "lib/srfi" #`",|srfi|.scm")
      (^[]
        (print ";; Generated automatically.  Do not edit.")
        (print #`"(define-module srfi.,|srfi| (extend srfi-,|srfi|))")))))

(define (main args)
  (match (cdr args)
    [(top_builddir) (generate top_builddir)]
    [else (exit 1 "Usage: gosh generate-srfi.scm $top_builddir")])
  0)
