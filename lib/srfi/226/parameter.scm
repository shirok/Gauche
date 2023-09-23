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

(define-syntax parameterize-srfi-226
  (er-macro-transformer
   (^[f r c]
     (define %parameterize
       ((with-module gauche.internal make-identifier)
        '%parameterize
        (find-module 'gauche.internal)
        '()))
     (match f
       [(_ () . body) (quasirename r `(let () ,@body))]
       ;; TODO: shortcut for single-parameter case
       [(_ ((param val) ...) . body)
        (quasirename r
          `(,%parameterize (list ,@param) (list ,@val) (^[] ,@body) #f))]
       [(_ . x) (error "Invalid parameterize form:" f)]))))
