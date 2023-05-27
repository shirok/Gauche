;;;
;;; (srfi 226 promise)
;;;

(define-module srfi.226.promise
  (export &uncaught-exception
          make-uncaught-exception-condition
          uncaught-exception-condition?
          uncaught-exception-condition-reason
          delay
          make-promise
          promise?
          force))
