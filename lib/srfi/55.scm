;;;
;;; SRFI-55   require-extension
;;;
;;; Written by Shiro Kawai
;;;

;; This file is to be autoloaded

(define-module srfi.55
  (use util.match)
  (export require-extension))
(select-module srfi.55)

;; We expand require-extension into cond-expand, which will
;; load and imports required features if necessary.
;; (This depends on the fact that Gauche's cond-expand has an effect
;; after the form).

(define-syntax require-extension
  (er-macro-transformer
   (^[f r c]
     (define (require-clause clause)
       (match clause
         [('srfi ns ...)
          (map (^n (quasirename r
                     `(cond-expand
                       [(library (,'srfi ,n))
                        (use ,(string->symbol #"srfi.~n"))]
                       [else
                        (errorf "Required srfi-~a isn't available." ',n)])))
               ns)]
         [_
          (error "malformed require-extension:" f)]))
     (match f
       [(_ clause ...)
        (quasirename r
          `(begin ,@(append-map require-clause clause)))]
       [_ (error "malformed require-extension:" f)]))))
