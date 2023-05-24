;;;
;;; (srfi 226 exception)
;;;

(define-module srfi.226.exception
  (export with-exception-handler
          exception-handler-stack
          raise
          raise-continuable
          guard
          =>
          else))
