;;;
;;; srfi-129 - Titlecase procedures
;;;

;; This is just an adaptor.

(define-module srfi-129
  (use gauche.unicode)
  (export char-title-case?
          char-titlecase
          string-titlecase)
  ;; NB: This is a temporary workaround of a bug regarding inherited modules
  ;; and transitive exports.
  ;; See the discussion of https://github.com/shirok/Gauche/issues/472
  (define string-titlecase (with-module gauche.unicode string-titlecase))
  )

