(define-library (srfi-159 unicode)
  (import (scheme base) (srfi-159 inner-base) (srfi 151))
  (export as-unicode unicode-terminal-width)
  (include "unicode.scm"))
