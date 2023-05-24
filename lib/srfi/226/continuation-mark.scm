;;;
;;; (srfi 226 continuation-mark)
;;;

(define-module srfi.226.continuation-mark
  (export &continuation
          make-continuation-violation
          continuation-violation?
          continuation-violation-prompt-tag
          with-continuation-mark
          with-continuation-marks
          call-with-immediate-continuation-mark
          continuation-marks
          current-continuation-marks
          continuation-mark-set?
          continuation-mark-set->list
          ;;continuation-mark-set->list*
          ;;continuation-mark-set->iterator
          continuation-mark-set-first
          make-continuation-mark-key
          continuation-mark-key?))
