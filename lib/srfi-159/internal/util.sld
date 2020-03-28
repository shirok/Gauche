(define-library (srfi-159 internal util)
  (export downcased
          fitted fitted/both fitted/left fitted/right fl
          joined joined/dot joined/last joined/prefix joined/range joined/suffix
          nl
          padded padded/both padded/left padded/right
          space-to
          tab-to trimmed trimmed/both trimmed/lazy trimmed/left trimmed/right
          upcased)
  (import (scheme base) (scheme char) (scheme write)
          (srfi-159 internal base))
  (include "util.scm"))
