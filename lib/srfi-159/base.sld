(define-library (srfi-159 base)
  (export call-with-output
          displayed
          each each-in-list escaped
          fitted fitted/both fitted/right fl fn forked
          joined joined/dot joined/last joined/prefix joined/range joined/suffix
          maybe-escaped
          nl nothing numeric numeric/comma numeric/fitted numeric/si
          padded padded/both padded/right pretty pretty-simply
          show space-to
          tab-to trimmed trimmed/both trimmed/lazy trimmed/right
          with with! written written-simply)
  (import (srfi-159 internal base)
          (srfi-159 internal pretty)
          (srfi-159 internal util)))
