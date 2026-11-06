;;;
;;; (srfi 226 exception)
;;;

(define-module srfi.226.exception
  (use scheme.base :only (raise) :prefix r7rs:)
  (export with-exception-handler
          exception-handler-stack
          (rename r7rs:raise raise)
          raise-continuable
          guard
          =>
          else))
