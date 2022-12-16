;;;
;;; srfi/29.scm - Localization
;;;

;; SRFI-29 defines the 'format' function in a way incompatible to
;; Gauche's (and CommonLisp's).   Srfi-29's ~N@* only affects
;; the following directive.   CommonLisp and Gauche's ~N@* affects
;; the rest of directives.
;;
;; So I splitted SRFI-29's 'format' functionality from the rest.
;; If you wish complete compatibility to SRFI-29, just load SRFI-29,
;; which overrides Gauche's native 'format'.
;; If you only wish SRFI-29's bundle API, but want to keep Gauche's
;; native 'format', use srfi.bundle instead.

(define-module srfi.29
  (extend srfi.29.bundle srfi.29.format))
