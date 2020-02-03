(define-library (srfi-159 util)
  (export
   nl fl space-to tab-to
   padded padded/left padded/right padded/both
   trimmed trimmed/left trimmed/right trimmed/both trimmed/lazy
   fitted fitted/left fitted/right fitted/both
   joined joined/prefix joined/suffix joined/last joined/dot joined/range
   upcased downcased)
  (import (scheme base) (scheme char) (scheme write)
          (srfi-159 inner-base))
  (include "show.scm"))
