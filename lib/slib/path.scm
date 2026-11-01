;;;
;;; Slib auxiliary utilities
;;;

;; This is included from slib.scm.   Also used to check slib availability
;; at runtime.

(define-module slib.path
  (export slib-library-path
          slib-available?))
(select-module slib.path)

(define (slib-library-path)
  (or
   ;; Use this getenv if your implementation supports it.
   (and-let1 p (sys-getenv "SCHEME_LIBRARY_PATH")
     (string-append p "/"))
   ;; Original template slib.scm dispatches with (software-type), but
   ;; Gauche only supports windows and unix, so we directly dispatches
   ;; with it.
   (cond-expand
    [gauche.windows "C:\\SLIB\\"]
    [else (regexp-replace "[^\/]$"
                          (with-module gauche.internal SLIB_DIR)
                          (^m #"~(m 0)/"))])))

(define (slib-available?)
  (let1 require-path
      (string-append (slib-library-path) "require.scm")
    (sys-access require-path R_OK)))
