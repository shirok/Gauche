;;;
;;; (srfi 226 shift-reset)
;;;

(define-module srfi.226.call-in-initial-continuation
  (use gauche.threads)
  (export &uncaught-exception
          make-uncaught-exception-condition
          uncaught-exception-condition?
          uncaught-exception-condition-reason
          call-in-initial-continuation
   ))
