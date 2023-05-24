;;;
;;; (srfi 226 prompt)
;;;

(define-module srfi.226.prompt
  ;; All bindings are built-in
  (export &continuation
          make-continuation-violation
          continuation-violation?
          continuation-violation-prompt-tag
          make-continuation-prompt-tag
          default-continuation-prompt-tag
          continuation-prompt-tag?
          call-with-continuation-prompt
          abort-current-continuation))
