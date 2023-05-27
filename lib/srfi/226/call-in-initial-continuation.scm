;;;
;;; (srfi 226 shift-reset)
;;;

(define-module srfi.226.call-in-initial-continuation
  (export &uncaught-exception
          make-uncaught-exception-condition
          uncaught-exception-condition?
          uncaught-exception-condition-reason
          ;; call-in-initial-continuation
   ))
