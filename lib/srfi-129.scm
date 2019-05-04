;;;
;;; srfi-129 - Titlecase procedures
;;;

;; This is just an adaptor.

(define-module srfi-129
  (use gauche.unicode)
  (export char-title-case?
          char-titlecase
          string-titlecase)
  ;; Create a binding of string-titlecase in this module.
  (define string-titlecase (with-module gauche.unicode string-titlecase))
  )

