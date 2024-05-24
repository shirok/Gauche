;;
;; Run partcont test with Gasbichler-Sperber shift/reset implementation.
;;

(use gauche.test)
(use gauche.partcont-meta)

(test-start "partcont-gasbichler")

(define-syntax gauche-only
  (syntax-rules ()
    [(_ expr ...) (begin expr ...)]))

(include "partcont.scm")

(test-end)
