(define-library (srfi-159 columnar)
  (import (scheme base) (scheme char) (scheme file) (scheme write)
          (srfi 1) (srfi 117) (srfi 130)
          (only (gauche base) let-optionals*)
          (srfi-159 base) (srfi-159 util))
  (export
   call-with-output-generator call-with-output-generators
   string->line-generator
   tabular columnar show-columns wrapped wrapped/list wrapped/char
   justified line-numbers from-file)
  (include "column.scm"))
