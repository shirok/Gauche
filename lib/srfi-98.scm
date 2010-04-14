;;;
;;; SRFI-98 An interface to access environment variables
;;;  

;; Public domain.

(define-module srfi-98
  (export get-environment-variable
          get-environment-variables))

(define get-environment-variable sys-getenv)
(define get-environment-variables sys-environ->alist)
