
(define-library (chibi show column)
  (import (scheme base) (scheme char) (scheme file) (scheme write)
          (srfi 1) (srfi 117) (srfi 130)
          (only (gauche base) let-optionals*)
          (chibi show))
  (export
   call-with-output-generator call-with-output-generators
   string->line-generator
   tabular columnar show-columns wrapped wrapped/list wrapped/char
   justified line-numbers from-file)
  (include "column.scm"))
