;;;
;;; compat.norational - override '/' for compatibility
;;;

(define-module compat.norational
  (export /))
(select-module compat.norational)

(define / inexact-/)
