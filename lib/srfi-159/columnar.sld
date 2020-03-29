(define-library (srfi-159 columnar)
  (import (scheme base) (scheme char) (scheme file) (scheme write)
          (srfi 1) (srfi 117) (srfi 130)
          (only (gauche base) let-optionals*)
          (srfi-159 base) (srfi-159 internal util))
  (export call-with-output-generator call-with-output-generators columnar
          from-file
          justified
          line-numbers
          show-columns string->line-generator
          tabular
          wrapped wrapped/char wrapped/list)
  (include "column.scm"))
