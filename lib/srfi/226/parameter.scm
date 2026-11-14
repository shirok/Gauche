;;;
;;; (srfi 226 parameter)
;;;

(define-module srfi.226.parameter
  (use util.match)
  (export (rename make-shared-parameter make-parameter)
          make-thread-parameter
          parameter?
          (rename parameterize-srfi-226 parameterize)
          current-parameterization
          parameterization?
          call-with-parameterization
          temporarily))
(select-module srfi.226.parameter)

;; We don't allow compatible mode.
;; Other than that, this is the same as parameterize in libparam.scm.
;; Probably we want to refactor.
(define-syntax parameterize-srfi-226
  (er-macro-transformer
   (^[f r c]
     (define %parameterize
       ((with-module gauche.internal make-identifier)
        '%parameterize
        (find-module 'gauche.internal)
        '()))
     (match f
       [(_ ((param val) ...) . body)
        (quasirename r
          `(,%parameterize (list ,@param) (list ,@val) (^[] ,@body) #f))]
       [(_ . x) (error "Invalid parameterize form:" f)]))))
