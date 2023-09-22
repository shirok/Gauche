;;;
;;; (srfi 226 parameter)
;;;

(define-module srfi.226.parameter
  (export (rename make-shared-parameter make-parameter)
          make-thread-parameter
          parameter?
          parameterize
          current-parameterization
          parameterization?
          call-with-parameterization
          temporarily))
